(in-package #:sbcl-ircd)

(defvar *server-listener* nil)
(defvar *listener-thread* nil)
(defvar *cleanup-thread* nil)
(defvar *server-state-lock* (make-mutex :name "server-state-lock"))
(defvar *server-running* nil)
(defvar *server-port* *default-port*)
(defvar *server-host* *default-host*)
(defparameter *writer-pool-size* 4 "Number of writer threads for async output")
(defvar *writer-pool* nil "List of writer threads.")

(defun join-thread-if-needed (thread &key (timeout 2))
  "Join THREAD unless it is NIL or the current thread."
  (when (and thread (not (eq thread sb-thread:*current-thread*)))
    (handler-case
        (join-thread thread :timeout timeout :default nil)
      (error () nil))))

(defun flush-client-queue (client)
  "Drain and write all pending messages for CLIENT.  The per-client
WRITE-MUTEX serialises stream writes for a given client without blocking
SEND-RAW, which only ever enqueues on the lock-free SB-CONCURRENCY:QUEUE
and CASes the WRITE-QUEUED-P flag.

Messages are written straight from the queue to the stream; no intermediate
list is materialised."
  (with-mutex ((client-write-mutex client))
    (loop
      (let ((queue (client-message-queue client))
            (wrote nil))
        (handler-case
            (let ((stream (client-stream client)))
              (loop
                (multiple-value-bind (msg present-p) (sb-concurrency:dequeue queue)
                  (unless present-p (return))
                  (setf wrote t)
                  (write-string msg stream)))
              (when wrote
                ;; FINISH-OUTPUT waits for buffered output to drain; the SBCL
                ;; stream docs note FORCE-OUTPUT only *initiates* it.
                (finish-output stream)))
          (error ()
            ;; Stream is gone - the reader thread will clean up.
            (return-from flush-client-queue nil)))
        (cond
          (wrote)                       ; wrote something: loop for more
          (t
           ;; Queue empty.  Clear the flag and re-check; if a producer
           ;; raced us, try to grab ownership again, otherwise let the
           ;; new mailbox entry start a fresh flush.
           (setf (slot-value client 'write-queued-p) nil)
           (cond
             ((sb-concurrency:queue-empty-p (client-message-queue client))
              (return-from flush-client-queue t))
             ((eq nil (sb-ext:compare-and-swap
                       (slot-value client 'write-queued-p) nil t))
              ;; Re-acquired ownership; loop to drain new messages.
              )
             (t
              ;; Another producer already pushed a fresh mailbox entry; a
              ;; different writer will pick it up.
              (return-from flush-client-queue t)))))))))

(defvar *writer-shutdown-sentinel* '#:writer-shutdown
  "Object sent through the mailbox to ask a writer thread to exit.")

(defun writer-thread-loop ()
  "Writer thread main loop: block on the lock-free mailbox until a client
needs flushing, or until a shutdown sentinel arrives."
  (loop
    (let ((message (sb-concurrency:receive-message *ready-mailbox*)))
      (cond
        ((eq message *writer-shutdown-sentinel*) (return))
        ((null message) (return))            ; defensive
        (t (flush-client-queue message))))))

(defun ip-vector-to-string (vec)
  "Convert an IP address vector to string format."
  (if (and (vectorp vec) (= (length vec) 4))
      (format nil "~D.~D.~D.~D" (aref vec 0) (aref vec 1) (aref vec 2) (aref vec 3))
      "unknown"))

(defun make-client-stream (socket)
  "Create a character stream for socket communication.

:FULL buffering (not :LINE): IRC lines all end in CRLF, so :LINE buffering would
flush the OS buffer on every message - defeating the writer pool, which already
drains N queued messages and calls FINISH-OUTPUT exactly once.  With :FULL the N
messages coalesce into a single write(2) per drain batch.  Correctness is
unchanged because every flush path ends in FINISH-OUTPUT."
  (socket-make-stream socket
                       :input t
                       :output t
                       :buffering :full
                       :element-type 'character
                       :external-format :utf-8))

(defun connection-runner (client)
  "Dedicated client loop handling thread with proper resource cleanup."
  (let ((stream (client-stream client)))
    (unwind-protect
        (handler-case
            (loop
              (let ((line (read-line stream nil nil)))
                (cond
                  ((null line)
                   (return)) ; EOF, disconnect
                  ((not (check-client-message-rate client))
                   (log-warning (format nil "Message rate limit exceeded for ~A"
                                        (or (client-nick client) "unregistered")))
                   (return))
                  ((not (valid-irc-message-p line))
                   (log-warning (format nil "Invalid or too-long message from ~A: ~A octets"
                                        (or (client-nick client) "unregistered")
                                        (irc-message-octet-length line)))
                   (return))
                  (t
                   (update-client-activity client)
                   (let ((parsed (parse-irc-message line)))
                     (when parsed
                       (destructuring-bind (prefix cmd params) parsed
                         (declare (ignore prefix))
                         (when (eq (handle-command client cmd params) 'quit-connection)
                           (return)))))))))
          (sb-bsd-sockets:socket-error ()
            ;; The remote side closed the socket or we did during shutdown;
            ;; the unwind clauses below take care of bookkeeping.
            nil)
          (stream-error () nil)         ; our own socket-close races read-line
          (end-of-file () nil)
          (error (e)
            ;; During shutdown STOP-SERVER closes the fd out from under
            ;; read-line, surfacing as a generic "Bad file descriptor" error -
            ;; expected, not worth logging.
            (when *server-running*
              (log-error (format nil "Connection error on client ~A: ~A"
                                 (or (client-nick client) "unregistered") e)))))
      ;; Ensure cleanup happens even on error
      (cleanup-client-connection client)
      (remove-active-client client)
      (decrement-connection-count)
      (close-client-io client))))

(defun listener-loop (listener-socket)
  "Accepts incoming client connections in a continuous loop until shutdown."
  (handler-case
      (loop while *server-running* do
        (handler-case
            (let ((client-socket (socket-accept listener-socket)))
              (multiple-value-bind (peer-ip peer-port) (socket-peername client-socket)
                (let ((ip-str (ip-vector-to-string peer-ip)))
                  (cond
                    ((not (check-connection-limit))
                     (log-security (format nil "Connection rejected: Max connections reached from ~A:~A"
                                           ip-str peer-port))
                     (socket-close client-socket))
                    ((not (check-ip-rate-limit ip-str))
                     (log-security (format nil "Connection rejected: Rate limit exceeded from ~A:~A"
                                           ip-str peer-port))
                     (socket-close client-socket))
                    ((zline-match ip-str)
                     (log-security (format nil "Connection rejected: Z-lined IP ~A:~A"
                                           ip-str peer-port))
                     (socket-close client-socket))
                    (t
                     (spawn-client client-socket ip-str peer-port))))))
          (sb-bsd-sockets:socket-error (e)
            ;; STOP-SERVER closes the listener which makes accept(2) signal
            ;; SOCKET-ERROR; only log when we are still meant to be running.
            (when *server-running*
              (log-error (format nil "Accept error: ~A" e))
              (sleep 0.1)))
          (error (e)
            (when *server-running*
              (log-error (format nil "Accept error: ~A" e))
              (sleep 0.1)))))
    (error (e)
      (log-error (format nil "Listener thread crashed: ~A" e)))))

(defun spawn-client (client-socket ip-str peer-port)
  "Wrap CLIENT-SOCKET in a CLIENT object, register it, and start its thread."
  (let ((client nil)
        (registered nil))
    (handler-case
        (let* ((stream (make-client-stream client-socket))
               (new-client (make-instance 'client
                                          :socket client-socket
                                          :stream stream
                                          :host ip-str)))
          (setf client new-client)
          ;; Cloak before the client can JOIN or message, so the real host is
          ;; never broadcast unless the user explicitly opts out with MODE -x.
          (setf (client-cloak client) (compute-cloak ip-str))
          (pushnew #\x (client-umodes client))
          (add-active-client client)
          (increment-connection-count)
          (setf registered t)
          (log-info (format nil "Accepted connection from ~A:~A (Total: ~A)"
                            ip-str peer-port *connection-count*))
          (setf (client-thread client)
                (make-thread (lambda () (connection-runner client))
                             :name (format nil "irc-client-~A" peer-port)))
          client)
      (error (e)
        (when client
          (remove-active-client client))
        (when registered
          (decrement-connection-count))
        (ignore-errors (socket-close client-socket))
        (error e)))))

(defun cleanup-loop ()
  "Periodically closes idle connections and cleans up expired invites."
  (loop while *server-running* do
    (loop repeat 60 while *server-running*
          do (setf *current-time* (get-universal-time)) (sleep 1))
    (when *server-running*
      (dolist (client (remove-if-not #'client-idle-p (active-clients)))
        (log-info (format nil "Closing idle client: ~A" (or (client-nick client) "unregistered")))
        (close-client-io client))
      ;; Clean up expired invites without holding the channel registry lock
      ;; while acquiring per-channel locks.
      (let ((channels nil))
        (with-channels-lock
          (maphash (lambda (name chan)
                     (declare (ignore name))
                     (push chan channels))
                   *channels*))
        (dolist (chan channels)
          (with-channel-lock (chan)
            (cleanup-expired-invites chan))))
      ;; Bound the rate-limit table: discard buckets whose window has expired.
      (prune-ip-history))))

(defun shutdown-workers ()
  "Signal every writer to exit, join the listener / cleanup / writer threads, and
null out the thread and mailbox globals.  Shared by STOP-SERVER and the
START-SERVER error unwind; the caller holds *SERVER-STATE-LOCK* and has already
cleared *SERVER-RUNNING* (and closed the listener socket)."
  (when *ready-mailbox*
    (dotimes (i (length *writer-pool*))
      (sb-concurrency:send-message *ready-mailbox* *writer-shutdown-sentinel*)))
  (join-thread-if-needed *listener-thread*)
  (join-thread-if-needed *cleanup-thread*)
  (dolist (writer *writer-pool*)
    (join-thread-if-needed writer))
  (setf *listener-thread* nil
        *cleanup-thread* nil
        *writer-pool* nil
        *ready-mailbox* nil))

(defun start-server (&key (port *server-port*) (host *server-host*))
  "Starts the IRC Daemon."
  (with-mutex (*server-state-lock*)
    (if *server-running*
        (progn
          (format t "Server is already running.~%")
          (force-output))
        (let ((listener (make-instance 'inet-socket :type :stream :protocol :tcp)))
          (handler-case
              (progn
                (setf (sockopt-reuse-address listener) t)
                (socket-bind listener (make-inet-address host) port)
                (socket-listen listener 128)
                (setf *server-port* port
                      *server-host* host
                      *server-running* t
                      *server-listener* listener
                      *ready-mailbox* (sb-concurrency:make-mailbox :name "irc-ready")
                      *current-time* (get-universal-time)
                      ;; Fresh cloak secret per run so cloaks can't be reversed
                      ;; with a table precomputed against another run.
                      *cloak-secret* (make-cloak-secret))
                (setf *writer-pool*
                      (loop for i from 1 to *writer-pool-size*
                            collect (make-thread (lambda ()
                                                  (writer-thread-loop))
                                                :name (format nil "irc-writer-~D" i))))
                (setf *cleanup-thread*
                      (make-thread (lambda ()
                                     (cleanup-loop))
                                   :name "irc-cleanup"))
                (setf *listener-thread*
                      (make-thread (lambda ()
                                     (listener-loop listener))
                                   :name "irc-listener"))
                (log-info (format nil "IRC Server started on ~A:~A (~D writer threads)" host port *writer-pool-size*))
                (force-output)
                t)
            (error (e)
              (setf *server-running* nil)
              (ignore-errors (socket-close listener))
              (setf *server-listener* nil)
              (shutdown-workers)
              (error e)))))))

(defun stop-server ()
  "Stops the IRC Daemon cooperatively: close the listener socket so ACCEPT
errors out, close every client socket so READ-LINE returns NIL, and send a
shutdown sentinel per writer thread through the mailbox so each can exit."
  (with-mutex (*server-state-lock*)
    (when *server-running*
      (setf *server-running* nil)
      (when *server-listener*
        (handler-case
            (socket-close *server-listener*)
          (error () nil))
        (setf *server-listener* nil))
      ;; Close every client first so its reader thread starts unwinding, then
      ;; signal and join the workers (SHUTDOWN-WORKERS sends each writer its
      ;; shutdown sentinel), and finally join the per-connection threads.
      (let ((clients (active-clients)))
        (dolist (client clients)
          (close-client-io client))
        (shutdown-workers)
        (dolist (client clients)
          (join-thread-if-needed (client-thread client))))
      ;; Clear any stragglers after cooperative shutdown has had a chance to
      ;; run each connection's unwind cleanup.
      (clrhash *clients*)
      (clrhash *channels*)
      (clrhash *connections*)
      ;; Workers are joined; reset directly instead of racing atomic decrements.
      (setf *connection-count* 0)
      (log-info "IRC Server stopped.")
      (force-output)
      (close-log-stream)
      t)))

(in-package #:sbcl-ircd)

(defun command-line-port (arg default)
  "Parse ARG as a TCP port, accepting strings or integers."
  (flet ((valid (port)
           (if (and (integerp port) (<= 1 port 65535)) port default)))
    (etypecase arg
      (null default)
      (integer (valid arg))
      (string (valid (handler-case (parse-integer arg)
                       (parse-error () nil)))))))

(defun main (&rest args)
  "Entry point: start the server and block until shutdown.  Optional arguments
(from ARGS, else the process command line) are PORT and HOST.

Disables the interactive debugger first: a headless daemon must never drop into
the REPL debugger and hang on an unhandled condition.  With it disabled, an
escaping error prints a backtrace and exits.  The worker loops (listener,
writers, cleanup, per-connection) already trap their own errors, so this guards
only the main thread and any truly unexpected escape."
  (sb-ext:disable-debugger)
  (let* ((port *default-port*)
         (host *default-host*)
         (argv (if args args (rest sb-ext:*posix-argv*))))
    (when argv
      (setf port (command-line-port (first argv) port))
      (when (second argv)
        (setf host (second argv))))
    ;; Graceful teardown on SIGINT/SIGTERM.
    (flet ((shutdown (sig code scp)
             (declare (ignore sig code scp))
             (log-info "Received shutdown signal. Shutting down gracefully...")
             (stop-server)
             (sb-ext:exit :code 0)))
      (sb-sys:enable-interrupt sb-unix:sigint #'shutdown)
      (sb-sys:enable-interrupt sb-unix:sigterm #'shutdown))
    (handler-case
        (progn
          (log-info (format nil "Starting SBCL-IRCD on ~A:~A" host port))
          (start-server :port port :host host)
          ;; Keep the process alive by waiting on the listener thread.
          (let ((thread *listener-thread*))
            (when thread
              (join-thread thread))))
      (error (e)
        (log-error (format nil "Fatal error in main: ~A" e))
        (ignore-errors (stop-server))
        (sb-ext:exit :code 1)))))

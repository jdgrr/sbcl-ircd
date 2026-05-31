(in-package #:sbcl-ircd)

(defclass client ()
  ((socket :initarg :socket :accessor client-socket)
   (stream :initarg :stream :accessor client-stream)
   (host :initarg :host :initform "127.0.0.1" :accessor client-host)
   (nick :initform nil :accessor client-nick)
   (user :initform nil :accessor client-user)
   (realname :initform nil :accessor client-realname)
   (registered :initform nil :accessor client-registered)
   (channels :initform nil :accessor client-channels)
   (thread :initarg :thread :initform nil :accessor client-thread)
   (last-activity :initform (get-universal-time) :accessor client-last-activity)
   (message-count :initform 0 :accessor client-message-count)
   (message-window-start :initform (get-universal-time) :accessor client-message-window-start)
   ;; LOCK protects compound mutations of the CLIENT-CHANNELS list and any
   ;; future state-only fields.  The hot output path (SEND-RAW / FLUSH) no
   ;; longer touches it.
   (lock :initform (make-mutex :name "client-lock") :accessor client-lock)
   ;; WRITE-MUTEX is held only by the writer pool while it is flushing this
   ;; client's stream.  Holding it across the write serialises stream output
   ;; for a single client without blocking SEND-RAW, which writes to a
   ;; lock-free SB-CONCURRENCY:QUEUE.
   (write-mutex :initform (make-mutex :name "client-write")
                :accessor client-write-mutex)
   (message-queue :initform (sb-concurrency:make-queue :name "client-msgs")
                  :accessor client-message-queue)
   ;; WRITE-QUEUED-P is a CASable flag used as a "client is already pending in
   ;; the writer mailbox" hint, so we coalesce N messages into one wake-up.
   ;; Accessed only via SLOT-VALUE (COMPARE-AND-SWAP needs a slot place), so it
   ;; intentionally has no accessor.
   (write-queued-p :initform nil)
   ;; User modes, an immutable-snapshot char list (o oper, x host-cloak,
   ;; g caller-id) mutated only by the owning thread via
   ;; PUSHNEW/REMOVE+SETF, so other threads read it lock-free.  GAGGED is a
   ;; HIDDEN oper-imposed flag (deliberately not a visible umode) that silently
   ;; drops the client's outbound messages.  ACCEPTS is the +g caller-id allow
   ;; list (nicks).  CLOAK caches the +x masked host.  QUIT-MESSAGE is the
   ;; reason broadcast by CLEANUP-CLIENT-CONNECTION (set by QUIT / KILL / ZLINE).
   (umodes :initform nil :accessor client-umodes)
   (gagged :initform nil :accessor client-gagged)
   (accepts :initform nil :accessor client-accepts)
   (cloak :initform nil :accessor client-cloak)
   (quit-message :initform "Connection closed" :accessor client-quit-message)))

(defclass channel ()
  ((name :initarg :name :accessor channel-name)
   (topic :initform "" :accessor channel-topic)
   ;; For RPL_TOPICWHOTIME (333): who last set the topic (full nick!user@host)
   ;; and when (Unix time).  Meaningful only while TOPIC is non-empty.
   (topic-setter :initform "" :accessor channel-topic-setter)
   (topic-time :initform 0 :accessor channel-topic-time)
   (operators :initform nil :accessor channel-operators)
   (voiced :initform nil :accessor channel-voiced)
   (bans :initform nil :accessor channel-bans)
   (ban-exceptions :initform nil :accessor channel-ban-exceptions)
   (invites :initform nil :accessor channel-invites)
   (invite-exceptions :initform nil :accessor channel-invite-exceptions)
   ;; Fresh mutable list per channel; default +nt.
   (modes :initform (list #\n #\t) :accessor channel-modes)
   (creation-time :initform (universal-to-unix (get-universal-time)) :accessor channel-creation-time)
   (member-vector :initform #() :accessor channel-member-vector)
   (client-set :initform (make-hash-table :test 'eq :synchronized t) :accessor channel-client-set)
   (lock :initform (make-mutex :name "channel-lock") :accessor channel-lock)))

;; Security and performance configuration
(defparameter *max-connections* 1000 "Maximum number of concurrent client connections")
(defparameter *max-message-length* 512 "Maximum IRC message length per RFC 1459")
(defparameter *max-nick-length* 30 "Maximum nickname length")
(defparameter *max-channel-length* 50 "Maximum channel name length")
(defparameter *max-channels-per-client* 20 "Maximum channels a client can join")
(defparameter *max-topic-length* 200 "Maximum channel topic length")
(defparameter *max-realname-length* 50 "Maximum GECOS/realname length")
(defparameter *max-reason-length* 100 "Maximum PART/KICK/QUIT reason length")
(defparameter *max-text-length* 400 "Maximum PRIVMSG text payload length")
(defparameter *max-mask-length* 100 "Maximum ban/exception mask length")
(defparameter *connection-timeout* 300 "Connection timeout in seconds (5 minutes)")
(defparameter *invite-timeout* 300 "Invite expiration time in seconds (5 minutes)")
(defparameter *message-rate-limit* 10 "Maximum messages per second per client")
(defparameter *connection-rate-limit* 10 "Maximum connections per second per IP")
(defparameter *enable-logging* t "Enable logging")
(defparameter *log-file* "ircd.log" "Log file path")

;; OPER credentials: alist of (NAME . PASSWORD).  Empty by default, which
;; disables OPER entirely.  NOTE: passwords are compared as plaintext, so this
;; list is as sensitive as the secrets in it; populate it before START-SERVER.
(defparameter *oper-credentials* nil
  "Alist of (NAME . PASSWORD) accepted by the OPER command.")

;; Defaults are user-tunable special variables rather than constants.
(defparameter *default-port* 6667 "Default IRC port")
(defparameter *default-host* "0.0.0.0" "Default bind address")

;; Server-identity defaults.  These show up in numeric replies (welcome,
;; whois) and the message prefix on server-originated messages.
;; IMPORTANT: *SERVER-NAME* must be a valid IRC hostname (no spaces); it is
;; used as the source prefix in every server-originated message.  A space in
;; the prefix terminates it, turning e.g. ":my server 001 ..." into prefix
;; "my", command "server", which no IRC client will recognise.
(defparameter *server-name* "sbcl-ircd.local"
  "Hostname used as the source prefix in all server-originated IRC messages.
Must not contain spaces.")
(defparameter *server-version* "sbcl-ircd-1.2"
  "Version string advertised in RPL_YOURHOST and RPL_MYINFO.")
(defparameter *creation-date* "2026-05-25"
  "Creation date string advertised in RPL_CREATED.")
(defparameter *motd* '("Welcome to sbcl-ircd!")
  "Lines shown between RPL_MOTDSTART (375) and RPL_ENDOFMOTD (376).")

;; Thread-safe server state globals.  All four registries below are
;; SYNCHRONIZED hash tables (SBCL 7.14 "Hash Table Extensions"): single
;; GETHASH/(SETF GETHASH)/REMHASH calls are already atomic, so no wrapper
;; mutex is required.  For *compound* atomic operations (e.g. find-or-create
;; or check-then-update) we use SB-EXT:WITH-LOCKED-HASH-TABLE, which simply
;; borrows the table's own internal mutex - avoiding a second lock that could
;; deadlock against the table's synchronisation.
(defvar *clients*
  (make-hash-table :test 'equalp :size 1024 :synchronized t)
  "Registered nickname (case-insensitive string) -> CLIENT.")
(defvar *channels*
  (make-hash-table :test 'equalp :size 256 :synchronized t)
  "Channel name (case-insensitive string) -> CHANNEL.")
(defvar *connections*
  (make-hash-table :test 'eq :size 1024 :synchronized t)
  "Active CLIENT objects, including unregistered ones.")
(defvar *ip-connection-history*
  (make-hash-table :test 'equalp :size 256 :synchronized t)
  "IP -> (CONS COUNT TIMESTAMP) for connection rate limiting.")

;; DEFGLOBAL + FIXNUM lets SB-EXT:ATOMIC-INCF/DECF update this without a mutex.
(declaim (type fixnum *connection-count*))
(sb-ext:defglobal *connection-count* 0
  "Current number of active connections.  Mutated only via atomic ops.")

;; CL universal time counts from 1900-01-01; Unix/IRC timestamps count from
;; 1970-01-01.  All outgoing IRC timestamps must use Unix time.
(defconstant +unix-epoch+
  (encode-universal-time 0 0 0 1 1 1970 0)
  "CL universal-time of the Unix epoch (1970-01-01T00:00:00Z).")

(defun universal-to-unix (universal-time)
  "Convert a CL universal-time integer to a Unix timestamp integer."
  (declare (type integer universal-time))
  (- universal-time +unix-epoch+))

;; Coarse ~1 s clock, refreshed each second by the cleanup thread, so the
;; per-message hot path reads a FIXNUM global instead of calling
;; GET-UNIVERSAL-TIME (~58 ns) twice per inbound line.
(declaim (type fixnum *current-time*))
(sb-ext:defglobal *current-time* (get-universal-time)
  "Cached GET-UNIVERSAL-TIME value at ~1 s resolution.")

;; Hash-table-lock helpers resolve to WITH-LOCKED-HASH-TABLE, which takes
;; the table's built-in lock - exactly what is needed for atomic compound ops.
(defmacro with-clients-lock (&body body)
  `(sb-ext:with-locked-hash-table (*clients*)
     ,@body))

(defmacro with-channels-lock (&body body)
  `(sb-ext:with-locked-hash-table (*channels*)
     ,@body))

(defmacro with-connections-lock (&body body)
  `(sb-ext:with-locked-hash-table (*connections*)
     ,@body))

(defmacro with-client-lock ((client) &body body)
  `(with-mutex ((client-lock ,client))
     ,@body))

(defmacro with-channel-lock ((channel) &body body)
  `(with-mutex ((channel-lock ,channel))
     ,@body))

;; Thread-safe registry API.  The hash tables are synchronized, so single
;; lookups go through GETHASH directly; the only operation that needs a
;; WITH-LOCKED-HASH-TABLE wrapper is the compound find-or-create.
(declaim (inline find-client find-channel add-active-client remove-active-client))

(defun find-client (nick)
  (gethash nick *clients*))

(defun find-channel (name)
  (gethash name *channels*))

(defun get-or-create-channel (name)
  (with-channels-lock
    (or (gethash name *channels*)
        (setf (gethash name *channels*) (make-instance 'channel :name name)))))

(defun add-active-client (client)
  (setf (gethash client *connections*) t))

(defun remove-active-client (client)
  (remhash client *connections*))

(defun active-clients ()
  ;; CLHS 3.6 forbids mutation during traversal; take the table's lock
  ;; for a consistent snapshot.
  (with-connections-lock
    (loop for client being the hash-keys of *connections*
          collect client)))

(defun maybe-prune-empty-channel (channel)
  "Atomically REMHASH CHANNEL from the global registry if it currently has no
members.  Both *CHANNELS* and CHANNEL's own lock are taken so the check
cannot race a concurrent JOIN."
  (with-channels-lock
    (with-channel-lock (channel)
      (when (zerop (length (channel-member-vector channel)))
        (remhash (channel-name channel) *channels*)))))

(declaim (inline channel-operator-p has-voice-p on-channel-p))

(defun channel-operator-p (client channel)
  (member client (channel-operators channel)))

(defun has-voice-p (client channel)
  (member client (channel-voiced channel)))

(defun on-channel-p (client channel)
  (gethash client (channel-client-set channel)))

(declaim (inline umode-set-p client-oper-p))

(defun umode-set-p (client char)
  "True if user mode CHAR is set on CLIENT."
  (and (member char (client-umodes client)) t))

(defun client-oper-p (client)
  "True if CLIENT has successfully OPERed (umode +o)."
  (umode-set-p client #\o))

(defun split-string (char string)
  "Split STRING by CHAR, returning empty fields as empty strings."
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start pos)
        while pos
        do (setf start (1+ pos))))

;; Host cloaking secret.  Regenerated each START-SERVER so cloaks are stable
;; within a run but cannot be precomputed/reversed across restarts.  A plain
;; SXHASH cloak is reversible: the IPv4 space is tiny (2^32) and SXHASH is
;; keyless, so an attacker just builds a reverse table.  Salting an MD5 with a
;; per-server random secret closes that - without the secret the mapping is
;; unknowable, and MD5 here is a non-reversible mixer, not an auth primitive.
(defvar *cloak-secret* nil "Per-server random secret salting host cloaks.")

(defun make-cloak-secret ()
  "Return a fresh 128-bit random hex secret from a randomly-seeded state."
  (let ((rs (make-random-state t)))
    (with-output-to-string (s)
      (dotimes (i 16) (format s "~2,'0x" (random 256 rs))))))

(defun ensure-cloak-secret ()
  "Return the cloak secret, lazily generating one if START-SERVER hasn't."
  (or *cloak-secret* (setf *cloak-secret* (make-cloak-secret))))

(defun cloak-segment (label)
  "Keyed base-36 cloak token for network key LABEL: the high 32 bits of
MD5(secret:label).  Non-reversible without the secret; the same LABEL always
yields the same token, which is what lets shared network prefixes line up."
  (let ((d (sb-md5:md5sum-string (concatenate 'string (ensure-cloak-secret) ":" label))))
    (format nil "~(~36r~)"
            (logior (ash (aref d 0) 24) (ash (aref d 1) 16)
                    (ash (aref d 2) 8) (aref d 3)))))

(defun ipv4-octets (host)
  "Return the four octet strings of HOST when it is a dotted-decimal IPv4
literal, else NIL."
  (let ((parts (split-string #\. host)))
    (when (and (= (length parts) 4)
               (every (lambda (p)
                        (and (<= 1 (length p) 3)
                             (every #'digit-char-p p)
                             (<= 0 (parse-integer p) 255)))
                      parts))
      parts)))

(defun compute-cloak (host)
  "Stable, non-reversible cloak for HOST.  An IPv4 literal A.B.C.D is cloaked
*structure-preserving* as low.mid.high.ip, where LOW is unique to the address,
MID is shared by the whole /24 (A.B.C) and HIGH by the whole /16 (A.B).  That
keeps the real octets hidden yet lets channel ops express range bans by
left-wildcarding: *!*@*.mid.high.ip bans the /24, *!*@*.high.ip the /16.  A
non-IPv4 host gets a flat cloak.  Labels are neutral so they never imply oper
status, and every segment is salted with the per-server secret."
  (let ((octets (ipv4-octets host)))
    (if octets
        (let ((a (first octets))
              (b (second octets))
              (c (third octets)))
          (format nil "~a.~a.~a.ip"
                  (cloak-segment host)
                  (cloak-segment (format nil "~a.~a.~a" a b c))
                  (cloak-segment (format nil "~a.~a" a b))))
        (format nil "cloak-~a.~a" (cloak-segment host) *server-name*))))

(defun client-host-display (client)
  "Host shown in CLIENT's prefix and WHO/WHOIS: the +x cloak when set, else the
real host.  Ban matching always uses the real CLIENT-HOST, never this."
  (if (umode-set-p client #\x)
      (or (client-cloak client) (client-host client))
      (client-host client)))

(defun close-client-io (client)
  "Close CLIENT's socket cooperatively.  Per the SBCL manual, SOCKET-CLOSE also
closes the cached stream from SOCKET-MAKE-STREAM, so closing the socket alone
suffices.  Lives here (not server.lisp) so command handlers - KILL, ZLINE - can
force a disconnect without a backward dependency on the server layer."
  (handler-case
      (socket-close (client-socket client))
    (sb-bsd-sockets:socket-error () nil)
    (error () nil)))

(defun client-channel-names (client)
  "Return a fresh list snapshot of the channels CLIENT has joined."
  (with-client-lock (client)
    (copy-list (client-channels client))))

(defun add-channel-to-client (client name)
  "Record that CLIENT has joined the channel NAME (no broadcast)."
  (with-client-lock (client)
    (pushnew name (client-channels client) :test #'string-equal)))

(defun remove-channel-from-client (client name)
  "Record that CLIENT has left the channel NAME (no broadcast)."
  (with-client-lock (client)
    (setf (client-channels client)
          (delete name (client-channels client) :test #'string-equal))))

(defun comma-separated-values (string)
  "Return non-empty comma-separated fields from STRING."
  (delete "" (split-string #\, string) :test #'string=))

;; Connection management functions.  Counter is a DEFGLOBAL FIXNUM, so we
;; use SB-EXT:ATOMIC-INCF / ATOMIC-DECF (no mutex contention).  The limit
;; check is a single FIXNUM read; a slightly stale value can at worst admit
;; or reject one extra connection, which is acceptable.
(declaim (inline increment-connection-count
                 decrement-connection-count
                 check-connection-limit))

(defun increment-connection-count ()
  (sb-ext:atomic-incf *connection-count*))

(defun decrement-connection-count ()
  (sb-ext:atomic-decf *connection-count*))

(defun check-connection-limit ()
  (< *connection-count* *max-connections*))

(defun check-ip-rate-limit (ip)
  "Return T if a new connection from IP is within the per-second budget,
accounting for this call.  The bucket is (CONS COUNT TIMESTAMP); we take the
synchronized table's own lock for the atomic read-modify-write."
  (let ((now (get-universal-time)))
    (sb-ext:with-locked-hash-table (*ip-connection-history*)
      (let* ((bucket (gethash ip *ip-connection-history*))
             (count (if bucket (car bucket) 0))
             (timestamp (if bucket (cdr bucket) 0)))
        (cond
          ((>= (- now timestamp) 1)
           (setf (gethash ip *ip-connection-history*) (cons 1 now))
           t)
          ((< count *connection-rate-limit*)
           (setf (gethash ip *ip-connection-history*) (cons (1+ count) now))
           t))))))

(defun prune-ip-history ()
  "Drop rate-limit buckets whose 1-second window has long expired so
*IP-CONNECTION-HISTORY* cannot grow without bound under connections from many
distinct addresses.  A pruned IP simply starts a fresh window on its next
connection, which is identical to an expired bucket.  Called periodically by
the cleanup thread; uses the table's own lock for the read-modify-write."
  (let ((now (get-universal-time))
        (stale nil))
    (sb-ext:with-locked-hash-table (*ip-connection-history*)
      (maphash (lambda (ip bucket)
                 (when (>= (- now (cdr bucket)) 2)
                   (push ip stale)))
               *ip-connection-history*)
      (dolist (ip stale)
        (remhash ip *ip-connection-history*)))))

(defun check-client-message-rate (client)
  "Return T if CLIENT is under the per-second message budget, accounting for
this call.  Called once per inbound line by the client's own reader thread,
so the slot mutations need no lock."
  (let ((now *current-time*))
    (when (>= (- now (client-message-window-start client)) 1)
      (setf (client-message-count client) 0
            (client-message-window-start client) now))
    (when (< (client-message-count client) *message-rate-limit*)
      (incf (client-message-count client))
      t)))

(defun update-client-activity (client)
  "Update client's last activity timestamp."
  (setf (client-last-activity client) *current-time*))

(defun client-idle-p (client)
  "Return T if CLIENT has been idle for more than *CONNECTION-TIMEOUT* seconds."
  (> (- *current-time* (client-last-activity client))
     *connection-timeout*))

;; Input validation functions.  Use ASCII-strict predicates instead of
;; ALPHA-CHAR-P / ALPHANUMERICP because SBCL's standard predicates accept
;; the full Unicode letter/digit classes - IRC nick and channel grammars are
;; ASCII-only.
(declaim (inline ascii-letter-p ascii-digit-p nick-special-char-p))

(defun ascii-letter-p (char)
  (or (<= (char-code #\A) (char-code char) (char-code #\Z))
      (<= (char-code #\a) (char-code char) (char-code #\z))))

(defun ascii-digit-p (char)
  (<= (char-code #\0) (char-code char) (char-code #\9)))

(defun nick-special-char-p (char)
  (and (find char "[]\\`_^{|}") t))

(defun valid-nick-p (nick)
  "Return T if NICK matches RFC 1459's ASCII nick grammar."
  (and (stringp nick)
       (<= 1 (length nick) *max-nick-length*)
       (let ((c (char nick 0)))
         (or (ascii-letter-p c) (nick-special-char-p c)))
       (every (lambda (c)
                (or (ascii-letter-p c)
                    (ascii-digit-p c)
                    (nick-special-char-p c)
                    (char= c #\-)))
              nick)))

(defun valid-channel-name-p (name)
  "Return T if NAME is a syntactically valid channel name (#[A-Za-z0-9_-]+
up to *MAX-CHANNEL-LENGTH*).  Restricted to ASCII to match VALID-NICK-P."
  (and (stringp name)
       (<= 2 (length name) *max-channel-length*)
       (char= (char name 0) #\#)
       (loop for i from 1 below (length name)
             for c = (char name i)
             always (or (ascii-letter-p c)
                        (ascii-digit-p c)
                        (char= c #\-)
                        (char= c #\_)))))

(defun utf8-octet-length (string)
  "UTF-8 byte length of STRING without allocating an octet buffer.
TYPECASE over the two *concrete* specialised string types (SIMPLE-CHARACTER and
SIMPLE-BASE - not the SIMPLE-STRING union, whose element type is still unknown
to the compiler) lets each hot-path branch compile a bounds-checked-free vector
loop with a known element type.  The general STRING arm is a correctness
fallback for the rare non-simple string."
  (declare (type string string) (optimize (speed 3) (safety 0)))
  (macrolet ((count-octets (s)
               `(let ((n 0))
                  (declare (type fixnum n))
                  (loop for ch across ,s
                        for code of-type fixnum = (char-code ch)
                        do (incf n (cond ((< code #x80) 1)
                                         ((< code #x800) 2)
                                         ((< code #x10000) 3)
                                         (t 4))))
                  n)))
    (typecase string
      ((simple-array character (*)) (count-octets string))
      ;; base-char in SBCL is 7-bit ASCII (code <= 127), so every char is 1 octet.
      ((simple-array base-char (*)) (length string))
      (t (count-octets string)))))

(defun irc-message-octet-length (message)
  "Return the UTF-8 wire length including the CRLF (or LF if CR present)."
  (+ (utf8-octet-length message)
     (if (and (plusp (length message))
              (char= (char message (1- (length message))) #\Return))
         1
         2)))

(defun valid-irc-message-p (message)
  "Validate IRC message format and the RFC 512-octet wire length limit."
  (and (stringp message)
       (<= (irc-message-octet-length message) *max-message-length*)
       (not (find #\Null message))
       (not (find #\Newline message))
       (let ((return-pos (position #\Return message)))
         (or (null return-pos)
             (= return-pos (1- (length message)))))))

(defun sanitize-string (str max-length)
  "Return STR with NUL/CR/LF removed and length capped at MAX-LENGTH.
Returns NIL if STR is not a string."
  (when (stringp str)
    (let* ((stripped (remove-if (lambda (c)
                                  (or (char= c #\Return)
                                      (char= c #\Newline)
                                      (char= c #\Null)))
                                str)))
      (if (> (length stripped) max-length)
          (subseq stripped 0 max-length)
          stripped))))

;; Ban mask matching functions
(defun wildcard-match (pattern string)
  "Return T if STRING matches the glob PATTERN.  '*' matches any run of
characters; '?' matches any single character.  Uses iterative backtracking
on '*' so worst-case complexity is O(|pattern| * |string|).  Matching is
case-insensitive to match the advertised ASCII IRC casemapping."
  (let ((p-len (length pattern))
        (s-len (length string))
        (p 0)
        (s 0)
        (star-idx nil)
        (match 0))
    (loop while (< s s-len) do
      (cond
        ((and (< p p-len)
              (or (char= (char pattern p) #\?)
                  (char-equal (char pattern p) (char string s))))
         (incf p)
         (incf s))
        ((and (< p p-len) (char= (char pattern p) #\*))
         (setf star-idx p match s)
         (incf p))
        (star-idx
         (incf match)
         (setf p (1+ star-idx) s match))
        (t (return-from wildcard-match nil))))
    (loop while (and (< p p-len) (char= (char pattern p) #\*))
          do (incf p))
    (= p p-len)))

(defun user-matches-ban (client ban-mask)
  "Return T if CLIENT's full nick!user@host matches BAN-MASK (with * and ?
wildcards).  Missing components in the mask default to \"*\".  The host pattern
is tested against BOTH the real host and the cloak, so a ban placed against the
visible (cloaked) host fires, while operator IP bans against the real host also
fire."
  (let* ((bang (position #\! ban-mask))
         (nick-pattern (if bang (subseq ban-mask 0 bang) ban-mask))
         (user-host (if bang (subseq ban-mask (1+ bang)) "*@*"))
         (at (position #\@ user-host))
         (user-pattern (if at (subseq user-host 0 at) user-host))
         (host-pattern (if at (subseq user-host (1+ at)) "*")))
    (and (wildcard-match nick-pattern (or (client-nick client) "*"))
         (wildcard-match user-pattern (or (client-user client) "*"))
         (or (wildcard-match host-pattern (client-host client))
             (let ((cloak (client-cloak client)))
               (and cloak (wildcard-match host-pattern cloak)))))))

;; Z-lines: operator-imposed IP bans checked at accept time (server.lisp) and
;; when a new Z-line is added (commands.lisp kills matching clients).  A single
;; mutex guards the list; this is a low-frequency control-plane path, not a hot
;; path, so a plain lock is simpler and safer than a lock-free structure.
(defvar *zlines* nil "List of (IP-MASK . REASON); connections from matching IPs are refused.")
(defvar *server-bans-lock* (make-mutex :name "server-bans-lock"))

(defun add-zline (mask reason)
  "Record a Z-line for glob IP-MASK with REASON (idempotent on MASK)."
  (with-mutex (*server-bans-lock*)
    (pushnew (cons mask reason) *zlines* :test #'string= :key #'car)))

(defun zline-match (ip)
  "Return the matching (MASK . REASON) cons for IP, or NIL."
  (with-mutex (*server-bans-lock*)
    (find ip *zlines* :test (lambda (i z) (wildcard-match (car z) i)))))

(defun cleanup-expired-invites (chan)
  "Drop any expired invites from CHAN.  Caller holds (WITH-CHANNEL-LOCK CHAN)."
  (let ((now (get-universal-time)))
    (setf (channel-invites chan)
          (delete-if (lambda (invite)
                       (>= (- now (second invite)) *invite-timeout*))
                     (channel-invites chan)))))

;; Mailbox written by SEND-RAW (commands.lisp) and drained by the writer pool
;; (server.lisp).  Defined here because commands.lisp loads before server.lisp.
(defvar *ready-mailbox* nil
  "SB-CONCURRENCY:MAILBOX of CLIENTs that have pending output to flush.")

;; Logging.  A single mutex serialises writers so concurrent threads cannot
;; interleave a half-written line on stdout or in the log file, and the file
;; handle is cached so each log call is a write/flush, not open/write/close.
(defvar *log-lock* (make-mutex :name "log-lock"))
(defvar *log-stream* nil "Cached append stream to *LOG-FILE*, or NIL.")
(defvar *log-stream-path* nil "Pathname *LOG-STREAM* is currently open on.")

(defun close-log-stream ()
  "Close and forget the cached log stream, if any."
  (with-mutex (*log-lock*)
    (when (and *log-stream* (open-stream-p *log-stream*))
      (ignore-errors (close *log-stream*)))
    (setf *log-stream* nil *log-stream-path* nil)))

(defun ensure-log-stream ()
  "Return the cached append stream to *LOG-FILE*, (re)opening it if the path
changed or the handle was closed.  Caller must hold *LOG-LOCK*."
  (unless (and *log-stream*
               (open-stream-p *log-stream*)
               (equal *log-stream-path* *log-file*))
    (when (and *log-stream* (open-stream-p *log-stream*))
      (ignore-errors (close *log-stream*)))
    (setf *log-stream* (open *log-file* :direction :output
                                        :if-exists :append
                                        :if-does-not-exist :create)
          *log-stream-path* *log-file*))
  *log-stream*)

(defun log-message (level message)
  "Write MESSAGE at severity LEVEL to standard output and *LOG-FILE*."
  (when *enable-logging*
    (let ((entry (format nil "[~A] [~A] ~A~%" (get-universal-time) level message)))
      (with-mutex (*log-lock*)
        (write-string entry *standard-output*)
        (force-output *standard-output*)
        (handler-case
            (let ((stream (ensure-log-stream)))
              (write-string entry stream)
              (finish-output stream))
          (error ()
            ;; Drop the bad handle so the next call retries with a fresh open.
            (when *log-stream* (ignore-errors (close *log-stream*)))
            (setf *log-stream* nil *log-stream-path* nil)))))))

(defun log-info (message)     (log-message "INFO" message))
(defun log-warning (message)  (log-message "WARN" message))
(defun log-error (message)    (log-message "ERROR" message))
(defun log-security (message) (log-message "SECURITY" message))

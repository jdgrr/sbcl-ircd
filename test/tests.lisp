(in-package #:cl-user)

(defpackage #:sbcl-ircd-test
  (:use #:cl)
  (:import-from #:sbcl-ircd
                #:valid-nick-p
                #:valid-channel-name-p
                #:sanitize-string
                #:parse-irc-message
                #:format-irc-message
                #:*max-nick-length*
                #:*max-channel-length*
                #:*max-message-length*
                #:client-nick
                #:client-user
                #:client-host))

(in-package #:sbcl-ircd-test)

(defparameter *tests-run* 0)
(defparameter *tests-passed* 0)

(defmacro test (name expression)
  `(progn
     (incf *tests-run*)
     (let ((result (handler-case ,expression
                     (error (e)
                       (format t "  ERROR: ~A~%" e)
                       nil))))
       (cond
         (result
          (incf *tests-passed*)
          (format t "PASS ~A~%" ,name))
         (t
          (format t "FAIL ~A~%" ,name))))))

(defun make-mock-client (nick user host)
  "Build a minimally-initialised CLIENT instance for protocol unit tests."
  (let ((client (allocate-instance (find-class 'sbcl-ircd::client))))
    (setf (slot-value client 'sbcl-ircd::host) host
          (slot-value client 'sbcl-ircd::nick) nick
          (slot-value client 'sbcl-ircd::user) user
          (slot-value client 'sbcl-ircd::realname) nil
          (slot-value client 'sbcl-ircd::registered) nil
          (slot-value client 'sbcl-ircd::channels) nil
          (slot-value client 'sbcl-ircd::thread) nil
          (slot-value client 'sbcl-ircd::last-activity) (get-universal-time)
          (slot-value client 'sbcl-ircd::message-count) 0
          (slot-value client 'sbcl-ircd::message-window-start) (get-universal-time)
          (slot-value client 'sbcl-ircd::lock) (sb-thread:make-mutex :name "mock-client")
          (slot-value client 'sbcl-ircd::write-mutex) (sb-thread:make-mutex :name "mock-client-write")
          (slot-value client 'sbcl-ircd::message-queue) (sb-concurrency:make-queue :name "mock-msgs")
          (slot-value client 'sbcl-ircd::write-queued-p) nil
          (slot-value client 'sbcl-ircd::umodes) nil
          (slot-value client 'sbcl-ircd::gagged) nil
          (slot-value client 'sbcl-ircd::accepts) nil
          (slot-value client 'sbcl-ircd::cloak) nil
          (slot-value client 'sbcl-ircd::quit-message) "Connection closed")
    client))

(defun run-tests ()
  "Run validation tests for sbcl-ircd."
  (setf *tests-run* 0 *tests-passed* 0)
  ;; Nickname validation tests
  (test "Valid nick: Alice" (valid-nick-p "Alice"))
  (test "Valid nick: a1" (valid-nick-p "a1"))
  (test "Valid nick: [test]" (valid-nick-p "[test]"))
  (test "Valid nick: {braces}" (valid-nick-p "{braces}"))
  (test "Valid nick: pipe|user" (valid-nick-p "pipe|user"))
  (test "Invalid nick: 1abc" (not (valid-nick-p "1abc")))
  (test "Invalid nick: empty string" (not (valid-nick-p "")))
  (test "Invalid nick: with space" (not (valid-nick-p "test user")))

  ;; Channel name validation tests
  (test "Valid channel: #test" (valid-channel-name-p "#test"))
  (test "Valid channel: #a-b_c" (valid-channel-name-p "#a-b_c"))
  (test "Invalid channel: test" (not (valid-channel-name-p "test")))
  (test "Invalid channel: #" (not (valid-channel-name-p "#")))
  (test "Invalid channel: #a b" (not (valid-channel-name-p "#a b")))

  ;; IRC message parsing
  (test "Parse simple message"
        (equal (parse-irc-message "PING :test") '(nil "PING" ("test"))))
  (test "Parse message with prefix"
        (equal (parse-irc-message ":nick!user@host PRIVMSG #chan :hello")
               '("nick!user@host" "PRIVMSG" ("#chan" "hello"))))
  (test "Parse multi-param JOIN"
        (equal (parse-irc-message "JOIN #chan1,#chan2") '(nil "JOIN" ("#chan1,#chan2"))))

  ;; IRC message formatting
  (test "Format simple message"
        (string= (format-irc-message nil "PING" '("test"))
                 (format nil "PING :test~C~C" #\Return #\Newline)))
  (test "Format message with prefix"
        (string= (format-irc-message "nick!user@host" "PRIVMSG" '("#chan" "hello"))
                 (format nil ":nick!user@host PRIVMSG #chan :hello~C~C" #\Return #\Newline)))

  ;; CLI argument parsing
  (test "Command-line port parser accepts integer"
        (= (sbcl-ircd::command-line-port 6668 6667) 6668))
  (test "Command-line port parser accepts string"
        (= (sbcl-ircd::command-line-port "6669" 6667) 6669))
  (test "Command-line port parser keeps default on junk"
        (= (sbcl-ircd::command-line-port "irc" 6667) 6667))

  ;; Octet length / RFC limits
  (test "Valid message length" (sbcl-ircd::valid-irc-message-p "PING :test"))
  (test "Message too long"
        (not (sbcl-ircd::valid-irc-message-p (make-string 513 :initial-element #\a))))
  (test "Message with embedded null"
        (not (sbcl-ircd::valid-irc-message-p (format nil "PING ~A" (code-char 0)))))
  (test "Message with embedded newline"
        (not (sbcl-ircd::valid-irc-message-p (format nil "PING~Ctest" #\Newline))))
  (test "ASCII octet length includes CRLF"
        (= (sbcl-ircd::irc-message-octet-length "PING") 6))

  ;; Comma-separated values
  (test "CSV: single" (equal (sbcl-ircd::comma-separated-values "test") '("test")))
  (test "CSV: many" (equal (sbcl-ircd::comma-separated-values "a,b,c") '("a" "b" "c")))
  (test "CSV: empty filtered" (equal (sbcl-ircd::comma-separated-values "a,,c") '("a" "c")))
  (test "CSV: trailing comma" (equal (sbcl-ircd::comma-separated-values "a,b,") '("a" "b")))

  ;; Wildcard matching
  (test "Wildcard exact" (sbcl-ircd::wildcard-match "test" "test"))
  (test "Wildcard *" (sbcl-ircd::wildcard-match "test*" "test123"))
  (test "Wildcard ?" (sbcl-ircd::wildcard-match "test?" "test1"))
  (test "Wildcard complex" (sbcl-ircd::wildcard-match "*test*" "123test456"))
  (test "Wildcard is case-insensitive" (sbcl-ircd::wildcard-match "AL*" "alice"))
  (test "Wildcard no match" (not (sbcl-ircd::wildcard-match "test" "other")))

  ;; Ban mask matching
  (let ((mock (make-mock-client "testuser" "testuser" "127.0.0.1")))
    (test "Ban exact" (sbcl-ircd::user-matches-ban mock "testuser!testuser@127.0.0.1"))
    (test "Ban is case-insensitive" (sbcl-ircd::user-matches-ban mock "TESTUSER!*@*"))
    (test "Ban wildcard" (sbcl-ircd::user-matches-ban mock "test*!*@*"))
    (test "Ban no match" (not (sbcl-ircd::user-matches-ban mock "other!*@*")))
    (test "Ban extra separators do not error"
          (not (sbcl-ircd::user-matches-ban mock "other!u@h@extra"))))

  ;; Structure-preserving cloak: range bans still work on cloaked IPv4 hosts.
  ;; A bound secret makes COMPUTE-CLOAK deterministic for the run.
  (let ((sbcl-ircd::*cloak-secret* "unit-test-secret"))
    (flet ((cloaked (host)
             (let ((m (make-mock-client "n" "u" host)))
               (setf (slot-value m 'sbcl-ircd::cloak) (sbcl-ircd::compute-cloak host))
               m)))
      (let* ((c1 (cloaked "1.2.3.4"))
             (c2 (cloaked "1.2.3.200"))     ; same /24 as c1
             (c3 (cloaked "9.9.9.9"))       ; different /16
             (ck (sbcl-ircd::client-cloak c1))
             ;; Strip the per-address LOW segment -> "*.mid.high.ip" /24 mask.
             (range (concatenate 'string "*!*@*." (subseq ck (1+ (position #\. ck))))))
        (test "Cloak octets are hidden" (not (search "1.2.3" ck)))
        (test "Cloak /24 range bans the address itself"
              (sbcl-ircd::user-matches-ban c1 range))
        (test "Cloak /24 range bans a same-/24 neighbor"
              (sbcl-ircd::user-matches-ban c2 range))
        (test "Cloak /24 range spares a different /16"
              (not (sbcl-ircd::user-matches-ban c3 range))))))
  (test "Invalid IPv4-looking hosts get flat cloaks"
        (not (search ".ip" (sbcl-ircd::compute-cloak "999.1.1.1"))))

  (let ((sbcl-ircd::*enable-logging* nil))
    (test "Failed start leaves server stopped"
          (handler-case
              (progn
                (sbcl-ircd:start-server :port 0 :host "not-an-ip")
                nil)
            (error ()
              (not sbcl-ircd::*server-running*)))))

  ;; Default channel modes are +nt (no external messages + topic-locked).
  (let ((chan (make-instance 'sbcl-ircd::channel :name "#fresh")))
    (test "Default channel mode includes +n" (member #\n (sbcl-ircd::channel-modes chan)))
    (test "Default channel mode includes +t" (member #\t (sbcl-ircd::channel-modes chan))))

  ;; RPL_NAMREPLY (353) must split across lines to stay within 512 octets and
  ;; drop no member.  SEND-NAMES enqueues via SEND-RAW, which pokes
  ;; *READY-MAILBOX*; bind a real one so the producer side has somewhere to go.
  (let ((sbcl-ircd::*ready-mailbox* (sb-concurrency:make-mailbox :name "test-ready")))
    (let* ((chan (make-instance 'sbcl-ircd::channel :name "#big"))
           (members (loop for i below 40
                          collect (make-mock-client
                                   (format nil "nick~2,'0D-abcdefghijklmnopqr" i) "u" "h")))
           (asker (make-mock-client "asker" "u" "h")))
      (setf (sbcl-ircd::channel-member-vector chan) (coerce members 'simple-vector))
      (dolist (m members) (setf (gethash m (sbcl-ircd::channel-client-set chan)) t))
      (sbcl-ircd::send-names asker chan "asker")
      (let* ((lines (loop for (msg p) = (multiple-value-list
                                         (sb-concurrency:dequeue
                                          (sbcl-ircd::client-message-queue asker)))
                          while p collect msg))
             (replies (remove-if-not (lambda (l) (search " 353 " l)) lines)))
        (test "NAMES splits into multiple 353 lines" (>= (length replies) 2))
        (test "every 353 line is within 512 octets"
              (every (lambda (l) (<= (sbcl-ircd::utf8-octet-length l) 512)) replies))
        (test "NAMES drops no member"
              (every (lambda (m) (some (lambda (l) (search (client-nick m) l)) replies))
                     members))
        (test "NAMES ends with exactly one 366"
              (= 1 (count-if (lambda (l) (search " 366 " l)) lines))))))

  ;; TOPIC replies: no 331 on JOIN; 332+333 when a topic is set; 331 only on
  ;; an explicit TOPIC query of a topicless channel.
  (let ((sbcl-ircd::*ready-mailbox* (sb-concurrency:make-mailbox :name "test-topic")))
    (flet ((qlines (thunk)
             (let ((q (make-mock-client "q" "u" "h")))
               (funcall thunk q)
               (loop for (m p) = (multiple-value-list
                                  (sb-concurrency:dequeue
                                   (sbcl-ircd::client-message-queue q)))
                     while p collect m))))
      (let ((empty (make-instance 'sbcl-ircd::channel :name "#e"))
            (set   (make-instance 'sbcl-ircd::channel :name "#s")))
        (setf (sbcl-ircd::channel-topic set) "hello"
              (sbcl-ircd::channel-topic-setter set) "amy!u@host"
              (sbcl-ircd::channel-topic-time set) 1700000000)
        (let ((join-empty (qlines (lambda (q) (sbcl-ircd::send-channel-info q empty "q"))))
              (join-set   (qlines (lambda (q) (sbcl-ircd::send-channel-info q set "q"))))
              (query-empty (qlines (lambda (q) (sbcl-ircd::send-topic-reply q "q" empty)))))
          (test "JOIN with no topic sends no 331"
                (notany (lambda (l) (search " 331 " l)) join-empty))
          (test "JOIN with no topic sends no 332"
                (notany (lambda (l) (search " 332 " l)) join-empty))
          (test "JOIN with topic set sends 332" (some (lambda (l) (search " 332 " l)) join-set))
          (test "JOIN with topic set sends 333 with setter+time"
                (some (lambda (l) (and (search " 333 " l)
                                       (search "amy!u@host" l)
                                       (search "1700000000" l)))
                      join-set))
          (test "TOPIC query of topicless channel sends 331"
                (some (lambda (l) (search " 331 " l)) query-empty))))))

  (format t "~%~D tests run, ~D passed.~%" *tests-run* *tests-passed*)
  (= *tests-run* *tests-passed*))

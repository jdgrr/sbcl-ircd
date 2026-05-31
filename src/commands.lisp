(in-package #:sbcl-ircd)

;; --- IRC reply plumbing ----------------------------------------------------

(defun send-raw (client line)
  "Queue LINE for asynchronous delivery to CLIENT.  Lock-free fast path:
ENQUEUE on a lock-free SB-CONCURRENCY:QUEUE, then CAS the WRITE-QUEUED-P
flag from NIL to T and push to *READY-MAILBOX* only if we won that CAS, so
N pending lines coalesce into a single writer wake-up."
  (sb-concurrency:enqueue line (client-message-queue client))
  (when (eq nil (sb-ext:compare-and-swap
                 (slot-value client 'write-queued-p) nil t))
    (sb-concurrency:send-message *ready-mailbox* client)))

(defun send-reply (client prefix command params &key (force-trailing t))
  "Format an IRC reply and SEND-RAW it to CLIENT."
  (send-raw client
            (format-irc-message prefix command params
                                :force-trailing force-trailing)))

(declaim (inline send-numeric send-error client-nick*))
(defun send-numeric (client code params &key (force-trailing t))
  "Send a server-originated numeric reply to CLIENT."
  (send-reply client *server-name* code params :force-trailing force-trailing))

(defun send-error (client code params)
  "Send a server-originated numeric error reply to CLIENT."
  (send-numeric client code params))

(defun client-nick* (client)
  "CLIENT's nick for the leading target field of a numeric reply, or \"*\" when
the client has not registered one yet."
  (or (client-nick client) "*"))

;; Each numeric-error helper derives the leading target field from the recipient
;; CLIENT (always its own nick), so callers never thread the nick separately.
(defun send-needmoreparams (client command)
  "Send ERR_NEEDMOREPARAMS for COMMAND."
  (send-error client *err-needmoreparams*
              (list (client-nick* client) command "Not enough parameters")))

(defmacro require-params (params client command block &key (min 1))
  "Return from BLOCK after ERR_NEEDMOREPARAMS unless PARAMS has MIN entries."
  `(when (< (length ,params) ,min)
     (send-needmoreparams ,client ,command)
     (return-from ,block)))

(defun send-no-such-nick (client target)
  "Send ERR_NOSUCHNICK for TARGET."
  (send-error client *err-nosuchnick*
              (list (client-nick* client) (or target "") "No such nick/channel")))

(defun send-no-such-channel (client target)
  "Send ERR_NOSUCHCHANNEL for TARGET."
  (send-error client *err-nosuchchannel*
              (list (client-nick* client) (or target "") "No such channel")))

(defun send-not-on-channel (client target &optional (text "You're not on that channel"))
  "Send ERR_NOTONCHANNEL for TARGET."
  (send-error client *err-notonchannel* (list (client-nick* client) target text)))

(defun send-chanop-required (client target)
  "Send ERR_CHANOPRIVSNEEDED for TARGET."
  (send-error client *err-chanoprisneeded*
              (list (client-nick* client) target "You're not channel operator")))

(defun client-full-prefix (client)
  "Return CLIENT's IRC source prefix: nick!user@host."
  (format nil "~A!~A@~A"
          (or (client-nick client) "*")
          (or (client-user client) "*")
          (client-host-display client)))

(defun broadcast-to-channel (channel message)
  "Send MESSAGE to every member of CHANNEL."
  (loop for member across (channel-member-vector channel)
        do (send-raw member message)))

(defun broadcast-to-peers (client message &key include-self)
  "Send MESSAGE once to every client that shares a channel with CLIENT, deduped
through an EQ set so a peer in several shared channels is notified only once.
With INCLUDE-SELF, CLIENT itself is also notified.  Used for the NICK and QUIT
fan-outs, which must reach the whole neighbourhood exactly once."
  (let ((recipients (make-hash-table :test 'eq)))
    (when include-self
      (setf (gethash client recipients) t))
    (dolist (chan-name (client-channel-names client))
      (let ((chan (find-channel chan-name)))
        (when chan
          (loop for c across (channel-member-vector chan)
                do (setf (gethash c recipients) t)))))
    (loop for c being the hash-keys of recipients
          do (send-raw c message))))

(defun send-isupport (client nick)
  "Advertise server capabilities (RPL_ISUPPORT, 005) so clients skip
unsupported probes and learn the limits we actually enforce."
  (send-numeric client *rpl-isupport*
                (list nick
                      "CHANTYPES=#"
                      "CHANMODES=beI,,,imnt"
                      "PREFIX=(ov)@+"
                      "MODES=4"
                      (format nil "NICKLEN=~D" *max-nick-length*)
                      (format nil "CHANNELLEN=~D" *max-channel-length*)
                      (format nil "TOPICLEN=~D" *max-topic-length*)
                      (format nil "CHANLIMIT=#:~D" *max-channels-per-client*)
                      "CASEMAPPING=ascii"
                      "are supported by this server")))

(defun send-motd (client nick)
  "Send the MOTD; the RPL_ENDOFMOTD (376) terminator tells the client that
registration is complete, which clients (irssi) wait for before syncing."
  (send-numeric client *rpl-motdstart*
              (list nick (format nil "- ~A Message of the day -" *server-name*)))
  (dolist (line *motd*)
    (send-numeric client *rpl-motd* (list nick (format nil "- ~A" line))))
  (send-numeric client *rpl-endofmotd*
              (list nick "End of /MOTD command")))

(defun umode-string (client)
  "CLIENT's user modes as a \"+...\" token (\"+\" when none are set)."
  (format nil "+~{~A~}" (client-umodes client)))

(defun try-register (client)
  "Complete registration once both NICK and USER have been received."
  (when (and (client-nick client)
             (client-user client)
             (not (client-registered client)))
    (setf (client-registered client) t)
    (let ((nick (client-nick client)))
      (send-numeric client *rpl-welcome*
                    (list nick (format nil "Welcome to the Internet Relay Network ~A"
                                       (client-full-prefix client))))
      (send-numeric client *rpl-yourhost*
                    (list nick (format nil "Your host is ~A, running version ~A"
                                       *server-name* *server-version*)))
      (send-numeric client *rpl-created*
                    (list nick (format nil "This server was created ~A" *creation-date*)))
      (send-numeric client *rpl-myinfo*
                    (list nick *server-name* *server-version* "gox" "beIimnotv")
                    :force-trailing nil)
      ;; RPL_ISUPPORT (005) + MOTD (372/375/376); the 376 terminator is the
      ;; signal clients wait on to consider registration complete.
      (send-isupport client nick)
      (send-motd client nick)
      ;; Tell the client about the modes the server set at connect (the +x host
      ;; cloak).  Clients track their own modes only from MODE messages - never
      ;; from the cloaked prefix - so without this the client believes it has no
      ;; modes even though a MODE <nick> query would report +x.
      (when (client-umodes client)
        (send-reply client nick "MODE" (list nick (umode-string client))
                    :force-trailing nil)))))

(defmacro toggle-membership (adding item place &key (test '(function eql)))
  "Add ITEM to PLACE when ADDING is true, else remove it.  Return true only
when PLACE changed; caller holds the relevant lock."
  (let ((value (gensym "ITEM-"))
        (present (gensym "PRESENT-")))
    `(let* ((,value ,item)
            (,present (member ,value ,place :test ,test)))
       (cond
         ((and ,adding (not ,present))
          (push ,value ,place)
          t)
         ((and (not ,adding) ,present)
          (setf ,place (remove ,value ,place :test ,test))
          t)))))

(declaim (inline mode-flag-string))
(defun mode-flag-string (adding char)
  "Two-character mode token: \"+x\" when ADDING, \"-x\" otherwise."
  (let ((s (make-string 2)))
    (setf (char s 0) (if adding #\+ #\-)
          (char s 1) char)
    s))

(defun broadcast-channel-mode (client channel target mode-args)
  "Broadcast a MODE change (TARGET followed by MODE-ARGS) from CLIENT to every
member of CHANNEL, reusing the lock-free fan-out in BROADCAST-TO-CHANNEL."
  (broadcast-to-channel channel
    (format-irc-message (client-full-prefix client) "MODE" (cons target mode-args)
                        :force-trailing nil)))

;; --- user modes & operator privilege --------------------------------------

(defun require-oper (client)
  "Return T when CLIENT is an IRC operator; otherwise send ERR_NOPRIVILEGES
and return NIL.  Used as a guard clause by the oper-only commands."
  (or (client-oper-p client)
      (progn
        (send-error client *err-noprivileges*
                    (list (client-nick* client)
                          "Permission Denied - you're not an IRC operator"))
        nil)))

(defun apply-user-modes (client mode-changes)
  "Apply the +/- MODE-CHANGES string to CLIENT itself and return the list of
(ADDING . CHAR) changes that actually took effect, in order, for echoing.
Self-settable modes are g/x; +o is reachable only through OPER, though a user
may remove their own +o.  Setting +x computes the host cloak on first use."
  (let ((adding t) (applied nil))
    (loop for ch across mode-changes do
      (case ch
        (#\+ (setf adding t))
        (#\- (setf adding nil))
        ((#\g #\x)
         (when (if adding (not (umode-set-p client ch)) (umode-set-p client ch))
           (cond
             (adding
              (when (and (char= ch #\x) (null (client-cloak client)))
                (setf (client-cloak client) (compute-cloak (client-host client))))
              (pushnew ch (client-umodes client)))
             (t (setf (client-umodes client) (remove ch (client-umodes client)))))
           (push (cons adding ch) applied)))
        (#\o
         ;; +o cannot be self-granted (use OPER); -o self-deop is allowed.
         (when (and (not adding) (umode-set-p client #\o))
           (setf (client-umodes client) (remove #\o (client-umodes client)))
           (push (cons nil #\o) applied)))))
    (nreverse applied)))

(defun broadcast-umode-change (client applied)
  "Echo the APPLIED (ADDING . CHAR) umode changes back to CLIENT as a MODE line,
collapsing runs of the same sign (e.g. \"+ox\", \"+i-g\")."
  (when applied
    (let ((s (with-output-to-string (out)
               (let ((sign nil))
                 (dolist (chg applied)
                   (let ((want (if (car chg) #\+ #\-)))
                     (unless (eql sign want)
                       (write-char want out)
                       (setf sign want))
                     (write-char (cdr chg) out)))))))
      (send-reply client (client-nick client) "MODE"
                  (list (client-nick client) s) :force-trailing nil))))

(defun handle-nick (client params)
  "Handle NICK command - change or set nickname."
  (require-params params client "NICK" handle-nick)

  (let ((new-nick (sanitize-string (car params) *max-nick-length*)))
    (cond
      ((or (null new-nick) (not (valid-nick-p new-nick)))
       (send-error client *err-erroneousnickname* (list (client-nick* client) (or new-nick "") "Erroneous nickname")))
      (t
       ;; CLAIM-NICK / CHANGE-NICK re-check availability *inside* the registry
       ;; lock and return NIL on collision, so the check-then-set is atomic;
       ;; an outside-the-lock NICK-IN-USE-P test would race two clients onto
       ;; the same nick and clobber one's registry entry.
       (let* ((old-nick (client-nick client))
              (ok (if old-nick
                      (change-nick client old-nick new-nick)
                      (claim-nick client new-nick))))
         (if ok
             (try-register client)
             (send-error client *err-nicknameinuse*
                         (list (or old-nick "*") new-nick "Nickname is already in use"))))))))

(defun change-nick (client old-nick new-nick)
  "Atomically re-key CLIENT from OLD-NICK to NEW-NICK in *CLIENTS* and broadcast
the NICK change to every channel CLIENT is in (plus CLIENT itself).  Returns T
on success, NIL if NEW-NICK is already held by another client (checked under the
registry lock so the check-then-set cannot race).  The lock is held only for the
O(1) re-key; recipient gathering happens afterwards under per-channel locks and
de-duplicates through an EQ set."
  (let ((old-prefix (client-full-prefix client)))
    (unless (with-clients-lock
              (let ((existing (gethash new-nick *clients*)))
                (cond
                  ((and existing (not (eq existing client))) nil)
                  (t (remhash old-nick *clients*)
                     (setf (client-nick client) new-nick
                           (gethash new-nick *clients*) client)
                     t))))
      (return-from change-nick nil))
    (broadcast-to-peers client
                        (format-irc-message old-prefix "NICK" (list new-nick))
                        :include-self t)
    t))

(defun claim-nick (client new-nick)
  "Atomically claim NEW-NICK for an as-yet-unregistered CLIENT.  Returns T on
success, NIL if another client already holds NEW-NICK (checked and set under the
registry lock so concurrent claimants cannot both win)."
  (with-clients-lock
    (let ((existing (gethash new-nick *clients*)))
      (cond
        ((and existing (not (eq existing client))) nil)
        (t (setf (client-nick client) new-nick
                 (gethash new-nick *clients*) client)
           t)))))

(defun handle-user (client params)
  "Handle USER command - set user information."
  (require-params params client "USER" handle-user :min 4)

  (when (client-registered client)
    (send-error client *err-alreadyregistered* (list (client-nick client) "Unauthorized command (already registered)"))
    (return-from handle-user))

  (setf (client-user     client) (sanitize-string (car   params) *max-nick-length*)
        (client-realname client) (sanitize-string (nth 3 params) *max-realname-length*))
  (try-register client))

(defun handle-ping (client params)
  "Handle PING command - respond with PONG."
  (require-params params client "PING" handle-ping)

  (send-reply client nil "PONG" (list *server-name* (car params)) :force-trailing t))

(defun handle-join (client params)
  "Handle JOIN command - join a channel."
  (require-params params client "JOIN" handle-join)

  (let ((nick (client-nick client)))
    (dolist (raw-chan-name (comma-separated-values (car params)))
      (let* ((chan-name (sanitize-string raw-chan-name *max-channel-length*))
             (existing (and chan-name (find-channel chan-name))))
        (cond
          ((or (null chan-name) (not (valid-channel-name-p chan-name)))
           (send-no-such-channel client chan-name))
          ((and (not (and existing (on-channel-p client existing)))
                (>= (length (client-channel-names client)) *max-channels-per-client*))
           (send-error client *err-toomanychannels* (list nick chan-name "You have joined too many channels")))
          (t
           (join-channel client chan-name nick)))))))

(defun attach-client-to-channel (client channel nick)
  "Add CLIENT to CHANNEL under the caller-held channel lock."
  (unless (on-channel-p client channel)
    (let ((is-first (zerop (length (channel-member-vector channel)))))
      (setf (channel-member-vector channel)
            (concatenate 'simple-vector (channel-member-vector channel)
                         (vector client)))
      (setf (gethash client (channel-client-set channel)) t)
      (when is-first
        (push client (channel-operators channel)))
      (setf (channel-invites channel)
            (remove-if (lambda (invite) (string= (car invite) nick))
                       (channel-invites channel)))
      t)))

(defun join-channel (client chan-name nick &optional force)
  "Join CHAN-NAME, creating it if new.  Access checks and membership mutation
share one channel-lock acquisition so bans/invites cannot change between them.
FORCE (operator SAJOIN) skips the ban / invite-only checks."
  (let ((chan (get-or-create-channel chan-name))
        (added nil)
        (error-code nil)
        (error-text nil))
    (with-channel-lock (chan)
      (unless (on-channel-p client chan)
        (when (and (not force) (plusp (length (channel-member-vector chan))))
          (cond
            ((some (lambda (ban) (user-matches-ban client ban)) (channel-bans chan))
             (setf error-code *err-bannedfromchan*
                   error-text "Cannot join channel (banned)"))
            ((member #\i (channel-modes chan))
             (cleanup-expired-invites chan)
             (unless (or (some (lambda (inv) (string= (car inv) nick)) (channel-invites chan))
                         (some (lambda (ex) (user-matches-ban client ex)) (channel-invite-exceptions chan)))
               (setf error-code *err-inviteonlychan*
                     error-text "Cannot join channel (invite-only)")))))
        (unless error-code
          (setf added (attach-client-to-channel client chan nick)))))
    (cond
      (error-code
       (send-error client error-code (list nick chan-name error-text)))
      (added
       (add-channel-to-client client chan-name)
       (broadcast-to-channel chan
         (format-irc-message (client-full-prefix client) "JOIN" (list chan-name)))
       (send-channel-info client chan nick)))))

(defun send-names (client channel nick)
  "Send the RPL_NAMREPLY (353) line(s) for CHANNEL followed by one
RPL_ENDOFNAMES (366).  Each member nick carries its @ (op) / + (voice) prefix,
and nicks are packed into as many 353 lines as needed so no reply exceeds the
RFC 512-octet limit (a single concatenated line overflows on a busy channel)."
  (let* ((name (channel-name channel))
         ;; Octet budget for the nicks payload = 512 minus the fixed framing of
         ;;   :<server> 353 <nick> = <name> :<nicks>\r\n
         ;; whose literal (non-variable) octets total 13: ':' + ' ' + "353" +
         ;; ' ' + ' ' + '=' + ' ' + ' ' + ':' + CRLF.
         (budget (max 1 (- *max-message-length*
                           (+ (utf8-octet-length *server-name*)
                              (utf8-octet-length nick)
                              (utf8-octet-length name)
                              13))))
         (buf (make-string-output-stream))
         (used 0)
         (any nil))
    (flet ((flush ()
             (send-numeric client *rpl-namreply*
                           (list nick "=" name (get-output-stream-string buf)))
             (setf used 0 any nil)))
      (loop for c across (channel-member-vector channel)
            for cn = (client-nick c)
            for pfx = (cond ((channel-operator-p c channel) #\@)
                            ((has-voice-p c channel) #\+))
            for tlen = (+ (if pfx 1 0) (utf8-octet-length cn))
            do (when (and any (> (+ used 1 tlen) budget))
                 (flush))
               (when any (write-char #\Space buf) (incf used))
               (when pfx (write-char pfx buf))
               (write-string cn buf)
               (incf used tlen)
               (setf any t))
      (when any (flush)))
    (send-numeric client *rpl-endofnames* (list nick name "End of NAMES list"))))

(defun send-current-topic (client nick channel)
  "Send RPL_TOPIC (332) followed by RPL_TOPICWHOTIME (333) - who set the topic
and when.  Caller must ensure CHANNEL actually has a topic set."
  (let ((name (channel-name channel)))
    (send-numeric client *rpl-topic* (list nick name (channel-topic channel)))
    (send-numeric client *rpl-topicwhotime*
                  (list nick name (channel-topic-setter channel)
                        (format nil "~D" (channel-topic-time channel)))
                  :force-trailing nil)))

(defun send-topic-reply (client nick channel)
  "Reply to a TOPIC *query*: RPL_NOTOPIC (331) when no topic is set, otherwise
RPL_TOPIC (332) + RPL_TOPICWHOTIME (333)."
  (if (string= (channel-topic channel) "")
      (send-numeric client *rpl-notopic*
                    (list nick (channel-name channel) "No topic is set"))
      (send-current-topic client nick channel)))

(defun send-channel-info (client channel nick)
  "Send JOIN info: per RFC 2812 section 3.2.1 the topic is sent ONLY when one is set
(RPL_TOPIC + RPL_TOPICWHOTIME) - no RPL_NOTOPIC on join - then the names list."
  (unless (string= (channel-topic channel) "")
    (send-current-topic client nick channel))
  (send-names client channel nick))

(defun handle-names (client params)
  "Handle NAMES command - list members of named channels.  SEND-NAMES already
emits the RPL_ENDOFNAMES terminator, so only the no-such-channel branch sends
its own (avoiding a duplicate 366)."
  (let ((nick (client-nick client)))
    (if (null params)
        (send-numeric client *rpl-endofnames* (list nick "*" "End of NAMES list"))
        (dolist (raw (comma-separated-values (car params)))
          (let* ((chan-name (sanitize-string raw *max-channel-length*))
                 (chan (and chan-name (find-channel chan-name))))
            (if chan
                (send-names client chan nick)
                (send-numeric client *rpl-endofnames*
                              (list nick (or chan-name "") "End of NAMES list"))))))))

(defun handle-part (client params)
  "Handle PART command - leave a channel."
  (require-params params client "PART" handle-part)

  (let ((reason (if (cdr params) (sanitize-string (cadr params) *max-reason-length*) "")))
    (dolist (raw-chan-name (comma-separated-values (car params)))
      (let* ((chan-name (sanitize-string raw-chan-name *max-channel-length*))
             (chan (and chan-name (find-channel chan-name))))
        (cond
          ((or (null chan-name) (null chan))
           (send-no-such-channel client chan-name))
          ((not (on-channel-p client chan))
           (send-not-on-channel client chan-name))
          (t
           (part-from-channel client chan chan-name reason)))))))

(defun detach-client-from-channel (client channel)
  "Remove CLIENT from every membership slot of CHANNEL (members, operators,
voiced, and the O(1) membership set) under the channel lock.  Shared by PART,
KICK and connection teardown so a departing user never lingers as an operator
or voiced member of a channel it has left."
  (with-channel-lock (channel)
    (setf (channel-member-vector channel) (remove client (channel-member-vector channel) :test #'eq)
          (channel-operators channel)     (remove client (channel-operators channel)     :test #'eq)
          (channel-voiced channel)        (remove client (channel-voiced channel)        :test #'eq))
    (remhash client (channel-client-set channel))))

(defun part-from-channel (client channel chan-name reason)
  "Remove a client from a channel and broadcast PART message."
  (let ((part-msg (format-irc-message (client-full-prefix client) "PART" (list chan-name reason))))
    (broadcast-to-channel channel part-msg)
    (detach-client-from-channel client channel))
  (remove-channel-from-client client chan-name)
  (maybe-prune-empty-channel channel))

(defun handle-kick (client params)
  "Handle KICK command - remove a user from a channel."
  (require-params params client "KICK" handle-kick :min 2)

  (let* ((chan-name (sanitize-string (car params) *max-channel-length*))
         (target-nick (cadr params))
         (reason (if (> (length params) 2) (sanitize-string (caddr params) *max-reason-length*) "Kicked"))
         (chan (find-channel chan-name)))
    (cond
      ((null chan)
       (send-no-such-channel client chan-name))
      ((not (on-channel-p client chan))
       (send-not-on-channel client chan-name))
      ((not (channel-operator-p client chan))
       (send-chanop-required client chan-name))
      (t
       (kick-user-from-channel client chan target-nick reason)))))

(defun kick-user-from-channel (kicker channel target-nick reason)
  "Kick a user from a channel."
  (let ((target-client (find-client target-nick)))
    (cond
      ((null target-client)
       (send-no-such-nick kicker target-nick))
      ((not (on-channel-p target-client channel))
       (send-not-on-channel kicker target-nick "They aren't on that channel"))
      (t
       (let ((kick-msg (format-irc-message (client-full-prefix kicker) "KICK"
                                           (list (channel-name channel) target-nick reason))))
         (broadcast-to-channel channel kick-msg)
         (detach-client-from-channel target-client channel)
         (remove-channel-from-client target-client (channel-name channel))
         (maybe-prune-empty-channel channel))))))

(defun handle-invite (client params)
  "Handle INVITE command - invite a user to a channel."
  (require-params params client "INVITE" handle-invite :min 2)

  (let* ((target-nick (car params))
         (chan-name (sanitize-string (cadr params) *max-channel-length*))
         (chan (find-channel chan-name)))
    (cond
      ((null chan)
       (send-no-such-channel client chan-name))
      ((not (on-channel-p client chan))
       (send-not-on-channel client chan-name))
      ((and (member #\i (channel-modes chan))
            (not (channel-operator-p client chan)))
       (send-chanop-required client chan-name))
      (t
       (invite-user-to-channel client target-nick chan)))))

(defun invite-user-to-channel (inviter target-nick channel)
  "Invite a user to a channel."
  (let ((target-client (find-client target-nick)))
    (cond
      ((null target-client)
       (send-no-such-nick inviter target-nick))
      (t
       (send-numeric inviter *rpl-inviting*
                     (list (client-nick inviter) target-nick (channel-name channel)))
       (send-reply target-client (client-full-prefix inviter) "INVITE"
                   (list target-nick (channel-name channel)) :force-trailing t)
       (with-channel-lock (channel)
         (pushnew (list target-nick *current-time*)
                  (channel-invites channel)
                  :test (lambda (a b) (string= (car a) (car b)))))))))

(defun privmsg-to-channel (client target text nick)
  "Deliver a PRIVMSG to a channel.  External users may write unless +n is set."
  (let ((chan (find-channel target)))
    (cond
      ((null chan)
       (send-no-such-channel client target))
      ((and (not (on-channel-p client chan))
            (member #\n (channel-modes chan)))
       (send-error client *err-cannotsendtochan* (list nick target "Cannot send to channel (+n)")))
      ((and (member #\m (channel-modes chan))
            (not (or (channel-operator-p client chan)
                     (has-voice-p client chan))))
       (send-error client *err-cannotsendtochan* (list nick target "Cannot send to channel (+m)")))
      (t
       (let ((msg (format-irc-message (client-full-prefix client) "PRIVMSG" (list target text))))
         (loop for member across (channel-member-vector chan)
             unless (eq member client) do (send-raw member msg)))))))

(defun privmsg-to-user (client target text nick)
  "Deliver a PRIVMSG to a single user.  A +g (caller-id) target only receives
messages from itself, from operators, and from senders on its ACCEPT list;
otherwise the message is held, the sender is told (716/717), and the target is
notified that someone tried to reach it (718)."
  (let ((target-client (find-client target)))
    (cond
      ((null target-client)
       (send-no-such-nick client target))
      ((and (umode-set-p target-client #\g)
            (not (eq target-client client))
            (not (client-oper-p client))
            (not (member nick (client-accepts target-client) :test #'string-equal)))
       (send-numeric client *rpl-targumodeg*
                     (list nick target "is in +g mode (server-side ignore)"))
       (send-numeric client *rpl-targnotify*
                     (list nick target "has been notified that you messaged them"))
       (send-numeric target-client *rpl-umodegmsg*
                     (list (client-nick target-client) (client-full-prefix client)
                           "is messaging you, and you have umode +g; /ACCEPT to allow")))
      (t
       (send-raw target-client
                 (format-irc-message (client-full-prefix client) "PRIVMSG" (list target text)))))))

(defun handle-privmsg (client params)
  "Handle PRIVMSG command - send a message to a list of users or channels.
A gagged client is silently ignored: its messages are dropped and it receives
no error, so it stays unaware of the gag (IRCX semantics)."
  (when (client-gagged client)
    (return-from handle-privmsg))
  (cond
    ((null params)
     (send-error client *err-norecipient* (list (client-nick client) "No recipient given (PRIVMSG)")))
    ((or (null (cdr params)) (string= (cadr params) ""))
     (send-error client *err-notexttosend* (list (client-nick client) "No text to send")))
    (t
     (let ((text (sanitize-string (cadr params) *max-text-length*))
           (nick (client-nick client)))
       (dolist (raw-target (comma-separated-values (car params)))
         (let ((target (sanitize-string raw-target *max-channel-length*)))
           (cond
             ((or (null target) (string= target ""))
              (send-no-such-nick client target))
             ((char= (char target 0) #\#)
              (privmsg-to-channel client target text nick))
             (t
              (privmsg-to-user client target text nick)))))))))

(defun handle-topic (client params)
  "Handle TOPIC command - query or set a channel's topic."
  (let* ((nick (client-nick client))
         (chan-name (and params (sanitize-string (car params) *max-channel-length*)))
         (chan (and chan-name (find-channel chan-name))))
    (cond
      ((null params)
       (send-needmoreparams client "TOPIC"))
      ((null chan)
       (send-no-such-channel client chan-name))
      ((not (on-channel-p client chan))
       (send-not-on-channel client chan-name))
      ((null (cdr params))
       (send-topic-reply client nick chan))
      (t
       (let ((new-topic (sanitize-string (cadr params) *max-topic-length*)))
         (cond
           ((and (member #\t (channel-modes chan))
                 (not (channel-operator-p client chan)))
            (send-chanop-required client chan-name))
           (t
            (with-channel-lock (chan)
              (setf (channel-topic chan) new-topic
                    (channel-topic-setter chan) (client-full-prefix client)
                    (channel-topic-time chan) (universal-to-unix *current-time*)))
            (broadcast-to-channel chan
              (format-irc-message (client-full-prefix client) "TOPIC"
                                  (list chan-name new-topic))))))))))

(defun mode-list-query-p (mode-string)
  "True if MODE-STRING is a pure list query: only list-type mode letters
(b ban, e ban-exception, I invite-exception), no +/- and at least one letter.
Such queries (e.g. irssi's MODE #c b on join) are readable by any user."
  (and (stringp mode-string)
       (plusp (length mode-string))
       (every (lambda (c) (find c "beI")) mode-string)))

(defun send-mode-lists (client chan target nick mode-string)
  "Emit the requested ban / ban-exception / invite-exception lists for CHAN,
then their terminators.  No operator privilege required (read-only)."
  (macrolet ((emit (accessor item-rpl end-rpl end-text)
               `(progn
                  (dolist (entry (,accessor chan))
                    (send-numeric client ,item-rpl (list nick target entry)))
                  (send-numeric client ,end-rpl (list nick target ,end-text)))))
    (loop for c across mode-string do
      (case c
        (#\b (emit channel-bans *rpl-banlist* *rpl-endofbanlist*
                   "End of channel ban list"))
        (#\e (emit channel-ban-exceptions *rpl-exceptlist* *rpl-endofexceptlist*
                   "End of channel ban exception list"))
        (#\I (emit channel-invite-exceptions *rpl-invitelist* *rpl-endofinvitelist*
                   "End of channel invite exception list"))))))

(defun apply-channel-modes (client chan target nick mode-changes args)
  "Apply a +/-mode string to CHAN for operator CLIENT, consuming ARGS for the
parameterised modes (o v b I/e) and broadcasting each accepted change.  Local
macros keep the per-letter cases free of lock/broadcast boilerplate."
  (let ((adding t))
    (flet ((need-more ()
             (send-needmoreparams client "MODE"))
           (no-such (n)
             (send-no-such-nick client n)))
      (macrolet
          ((flag-mode (ch place)
             `(when (with-channel-lock (chan)
                      (toggle-membership adding ,ch ,place))
                (broadcast-channel-mode client chan target
                                        (list (mode-flag-string adding ,ch)))))
           (member-mode (ch place)
             `(let ((mnick (pop args)))
                (if (null mnick)
                    (need-more)
                    (let ((mc (find-client mnick)))
                      (if (null mc)
                          (no-such mnick)
                          (let ((changed (with-channel-lock (chan)
                                           (if (on-channel-p mc chan)
                                               (toggle-membership adding mc ,place)
                                               :not-on-channel))))
                            (cond
                              ((eq changed :not-on-channel)
                               (no-such mnick))
                              (changed
                               (broadcast-channel-mode
                                client chan target
                                (list (mode-flag-string adding ,ch) mnick))))))))))
           (list-mode (ch place)
             `(let ((raw (pop args)))
                (if (null raw)
                    (send-mode-lists client chan target nick (string ,ch))
                    (let ((mask (sanitize-string raw *max-mask-length*)))
                      (if (or (null mask) (string= mask ""))
                          (need-more)
                          (when (with-channel-lock (chan)
                                  (toggle-membership adding mask ,place
                                                     :test #'string-equal))
                            (broadcast-channel-mode
                             client chan target
                             (list (mode-flag-string adding ,ch) mask)))))))))
        (loop for ch across mode-changes do
          (case ch
            (#\+ (setf adding t))
            (#\- (setf adding nil))
            (#\o (member-mode #\o (channel-operators chan)))
            (#\v (member-mode #\v (channel-voiced chan)))
            (#\t (flag-mode #\t (channel-modes chan)))
            (#\m (flag-mode #\m (channel-modes chan)))
            (#\n (flag-mode #\n (channel-modes chan)))
            (#\i (flag-mode #\i (channel-modes chan)))
            (#\b (list-mode #\b (channel-bans chan)))
            (#\e (list-mode #\e (channel-ban-exceptions chan)))
            (#\I (list-mode #\I (channel-invite-exceptions chan)))))))))

(defun handle-channel-mode (client params target nick)
  "Dispatch a channel-target MODE: full query, any-user list query, or the
operator-gated mutation path."
  (let ((chan (find-channel target)))
    (cond
      ((null chan)
       (send-no-such-channel client target))
      ((null (cdr params))
       (send-numeric client *rpl-channelmodeis*
                     (list nick target (format nil "+~{~A~}" (channel-modes chan))))
       ;; RPL_CREATIONTIME (329): irssi gates CHANNEL_QUERY_MODE on this; it
       ;; also validates the timestamp, so it MUST be a Unix epoch value, not
       ;; a CL universal-time (which would produce a year ~2096 and be rejected).
       (send-numeric client *rpl-creationtime*
                     (list nick target (format nil "~D" (channel-creation-time chan)))
                     :force-trailing nil))
      ;; List-only queries (MODE #c b/e/I, no value) are readable by any user:
      ;; clients fire them on join and block on the reply, so gating them behind
      ;; operator status hangs the join sync.
      ((and (null (cddr params)) (mode-list-query-p (cadr params)))
       (send-mode-lists client chan target nick (cadr params)))
      ((not (on-channel-p client chan))
       (send-not-on-channel client target))
      ((not (channel-operator-p client chan))
       (send-chanop-required client target))
      (t
       (apply-channel-modes client chan target nick (cadr params) (cddr params))))))

(defun handle-user-mode (client params target nick)
  "Handle a user-target MODE: a user may only query or change their own modes."
  (let ((tc (find-client target)))
    (cond
      ((null tc)
       (send-no-such-nick client target))
      ((not (eq tc client))
       (send-error client *err-usersdontmatch*
                   (list nick "Cannot change mode for other users")))
      ((null (cdr params))
       (send-numeric client *rpl-umodeis* (list nick (umode-string client))))
      (t
       (broadcast-umode-change client (apply-user-modes client (cadr params)))))))

(defun handle-mode (client params)
  "Dispatch MODE to channel-mode or user-mode handling."
  (let ((nick (client-nick client))
        (target (car params)))
    (cond
      ((null params)
       (send-needmoreparams client "MODE"))
      ((or (null target) (string= target ""))
       (send-no-such-nick client target))
      ((char= (char target 0) #\#)
       (handle-channel-mode client params target nick))
      (t
       (handle-user-mode client params target nick)))))

(defun handle-whois (client params)
  "Handle WHOIS command - return registration info for a nick."
  (let* ((nick (client-nick client))
         (target (and params (car params)))
         (target-client (and target (find-client target))))
    (cond
      ((null params)
       (send-needmoreparams client "WHOIS"))
      ((null target-client)
       (send-no-such-nick client target))
      (t
       (send-numeric client *rpl-whoisuser*
                     (list nick target
                           (client-user target-client)
                           (client-host-display target-client)
                           "*"
                           (client-realname target-client)))
       (send-numeric client *rpl-whoisserver*
                     (list nick target *server-name* *server-version*))
       (send-numeric client *rpl-endofwhois*
                     (list nick target "End of /WHOIS list"))))))

(defun who-flags (target-client channel)
  "Build the WHO flag field for TARGET-CLIENT: \"H\" (here; we have no away
state) plus \"@\"/\"+\" for channel operator/voiced status when CHANNEL is given."
  (cond ((null channel) "H")
        ((channel-operator-p target-client channel) "H@")
        ((has-voice-p target-client channel) "H+")
        (t "H")))

(defun send-who-reply (client target-client channel channel-label nick)
  "Send a single RPL_WHOREPLY (352) line describing TARGET-CLIENT."
  ;; <client> <channel> <user> <host> <server> <nick> <flags> :<hopcount> <realname>
  (send-numeric client *rpl-whoreply*
              (list nick
                    channel-label
                    (or (client-user target-client) "*")
                    (client-host-display target-client)
                    *server-name*
                    (or (client-nick target-client) "*")
                    (who-flags target-client channel)
                    (format nil "0 ~A" (or (client-realname target-client) "")))))

(defun handle-who (client params)
  "Handle WHO.  irssi (and most clients) fire WHO <channel> on join and block
the channel \"sync\" until RPL_ENDOFWHO (315) arrives, so this must always
terminate with a 315 even when there are no matches."
  (let* ((nick (client-nick client))
         (mask (and params (car params))))
    (cond
      ;; WHO #channel - list visible members of that channel.
      ((and mask (plusp (length mask)) (char= (char mask 0) #\#))
       (let ((chan (find-channel mask)))
         (when chan
           (loop for member across (channel-member-vector chan)
                 do (send-who-reply client member chan mask nick))))
       (send-numeric client *rpl-endofwho*
                   (list nick mask "End of /WHO list")))
      ;; WHO <nick-or-mask> - match a single nick (full mask matching is more
      ;; than clients need for channel sync; keep it simple and correct).
      ((and mask (plusp (length mask)))
       (let ((target (find-client mask)))
         (when target
           (send-who-reply client target nil "*" nick)))
       (send-numeric client *rpl-endofwho*
                   (list nick mask "End of /WHO list")))
      ;; Bare WHO - we don't enumerate the whole network; just close it out.
      (t
       (send-numeric client *rpl-endofwho*
                   (list nick "*" "End of /WHO list"))))))

(defun handle-quit (client params)
  "Handle QUIT command - disconnect client with optional reason."
  (let ((reason (if params (sanitize-string (car params) *max-reason-length*) "Client Quit")))
    (setf (client-quit-message client) (format nil "Quit: ~A" reason))
    (send-reply client nil "ERROR" (list (format nil "Closing Link: ~A (~A)" (client-host client) reason)) :force-trailing t)
    'quit-connection))

(defun cleanup-client-connection (client)
  "Gracefully remove CLIENT from every channel and from the global registry,
broadcasting a single QUIT to each user who still shares a channel with it."
  (let ((nick (client-nick client)))
    (when nick
      (let ((quit-msg (format-irc-message (client-full-prefix client) "QUIT"
                                          (list (client-quit-message client)))))
        ;; Detach from (and prune) every channel first, so the departing client
        ;; is excluded from its own QUIT, then notify the remaining neighbours.
        (dolist (chan-name (client-channel-names client))
          (let ((chan (find-channel chan-name)))
            (when chan
              (detach-client-from-channel client chan)
              (maybe-prune-empty-channel chan))))
        (broadcast-to-peers client quit-msg)
        (with-clients-lock
          ;; Only drop the entry if it still points at us; a reconnect under
          ;; the same nick may already have replaced it.
          (when (eq (gethash nick *clients*) client)
            (remhash nick *clients*)))
        (with-client-lock (client)
          (setf (client-channels client) nil))))))

(defun handle-cap (client params)
  "Handle CAP negotiation. This server supports no capabilities; REQ is always NAK'd."
  (when params
    (let ((subcmd (string-upcase (car params))))
      (cond
        ((string= subcmd "LS")
         (send-reply client *server-name* "CAP" (list "*" "LS" "") :force-trailing t))
        ((string= subcmd "REQ")
         (send-reply client *server-name* "CAP"
                     (list "*" "NAK" (if (cdr params) (cadr params) "")) :force-trailing t))
        ((string= subcmd "END")
         nil)))))

;; --- operator commands -----------------------------------------------------

(defmacro define-oper-command (name command min &body body)
  "Define an oper-only command handler NAME for IRC COMMAND requiring MIN params.
BODY runs only once the caller supplied enough params AND is an IRC operator, and
is evaluated with CLIENT, PARAMS and NICK (= CLIENT's nick) in scope.  Centralises
the ERR_NEEDMOREPARAMS / REQUIRE-OPER guard shared by every oper command.  A
leading string in BODY becomes the handler's docstring."
  (let ((doc (when (stringp (car body)) (list (pop body)))))
    `(defun ,name (client params)
       ,@doc
       (let ((nick (client-nick client)))
         (cond
           ((< (length params) ,min) (send-needmoreparams client ,command))
           ((not (require-oper client)))
           (t ,@body))))))

(defun handle-oper (client params)
  "Handle OPER <name> <password>: on a credential match, grant operator status
(umode +o) and announce it back to the client.  Host masking (+x) is already on
for every client from connect time, so OPER need not touch it."
  (let ((nick (client-nick client)))
    (cond
      ((< (length params) 2)
       (send-needmoreparams client "OPER"))
      (t
       (let ((entry (assoc (car params) *oper-credentials* :test #'string=)))
         (cond
           ((and entry (string= (cdr entry) (cadr params)))
            (pushnew #\o (client-umodes client))
            (send-numeric client *rpl-youreoper* (list nick "You are now an IRC operator"))
            (broadcast-umode-change client (list (cons t #\o)))
            (log-security (format nil "OPER granted to ~A (~A)" nick (client-full-prefix client))))
           (t
            (send-error client *err-passwdmismatch* (list nick "Password incorrect"))
            (log-security (format nil "OPER failed for name ~S from ~A"
                                  (car params) (client-host client))))))))))

(defun handle-accept (client params)
  "Handle ACCEPT for +g caller-id: comma-separated nicks are added, a leading
'-' removes, and ACCEPT * lists the current allow list."
  (let ((nick (client-nick client)))
    (cond
      ((null params)
       (send-needmoreparams client "ACCEPT"))
      ((string= (car params) "*")
       (when (client-accepts client)
         (send-numeric client *rpl-acceptlist*
                       (list nick (format nil "~{~A~^ ~}" (client-accepts client)))))
       (send-numeric client *rpl-endofaccept* (list nick "End of /ACCEPT list")))
      (t
       (dolist (entry (comma-separated-values (car params)))
         (if (and (plusp (length entry)) (char= (char entry 0) #\-))
             (setf (client-accepts client)
                   (delete (subseq entry 1) (client-accepts client) :test #'string-equal))
             (pushnew entry (client-accepts client) :test #'string-equal)))))))

(define-oper-command handle-kill "KILL" 1
  "KILL <nick> [reason]: forced disconnect.  Closing the target's socket makes
its reader thread unwind through CLEANUP-CLIENT-CONNECTION, which broadcasts the
QUIT-MESSAGE we set here."
  (let* ((target-nick (car params))
         (reason (if (cdr params) (sanitize-string (cadr params) *max-reason-length*) "Killed"))
         (target (find-client target-nick)))
    (if (null target)
        (send-no-such-nick client target-nick)
        (progn
          (log-security (format nil "KILL ~A by ~A (~A)" target-nick nick reason))
          (setf (client-quit-message target) (format nil "Killed by ~A (~A)" nick reason))
          (send-reply target nil "ERROR"
                      (list (format nil "Closing Link: ~A (Killed (~A))" nick reason))
                      :force-trailing t)
          (close-client-io target)))))

(define-oper-command handle-zline "ZLINE" 1
  "ZLINE <ip-mask> [reason]: IP ban.  Records the glob mask and immediately
disconnects every connected client whose real host matches."
  (let ((mask (sanitize-string (car params) *max-mask-length*))
        (reason (if (cdr params) (sanitize-string (cadr params) *max-reason-length*) "Z-lined")))
    (cond
      ((or (null mask) (string= mask ""))
       (send-needmoreparams client "ZLINE"))
      (t
       (add-zline mask reason)
       (log-security (format nil "ZLINE ~A by ~A (~A)" mask nick reason))
       (send-reply client *server-name* "NOTICE"
                   (list nick (format nil "Added Z-line for ~A: ~A" mask reason))
                   :force-trailing t)
       (dolist (c (active-clients))
         (when (wildcard-match mask (client-host c))
           (setf (client-quit-message c) (format nil "Z-lined: ~A" reason))
           (close-client-io c)))))))

(define-oper-command handle-gag "GAG" 1
  "GAG <nick> [ON|OFF] (default ON): silent gag toggle.  See HANDLE-PRIVMSG for
the drop semantics; the gagged user is never told."
  (let ((target (find-client (car params)))
        (on (not (and (cdr params) (string-equal (cadr params) "OFF")))))
    (if (null target)
        (send-no-such-nick client (car params))
        (progn
          (setf (client-gagged target) on)
          (log-security (format nil "GAG ~A ~A by ~A"
                                (client-nick target) (if on "ON" "OFF") nick))
          (send-reply client *server-name* "NOTICE"
                      (list nick (format nil "~A is now ~Agagged"
                                         (client-nick target) (if on "" "un")))
                      :force-trailing t)))))

(define-oper-command handle-sajoin "SAJOIN" 2
  "SAJOIN <nick> <channel>: forced join, bypassing ban and invite-only checks."
  (let ((target (find-client (car params)))
        (chan-name (sanitize-string (cadr params) *max-channel-length*)))
    (cond
      ((null target)
       (send-no-such-nick client (car params)))
      ((or (null chan-name) (not (valid-channel-name-p chan-name)))
       (send-no-such-channel client chan-name))
      (t
       (let ((tnick (client-nick target)))
         (join-channel target chan-name tnick t)
         (log-security (format nil "SAJOIN ~A to ~A by ~A" tnick chan-name nick)))))))

(define-oper-command handle-sapart "SAPART" 2
  "SAPART <nick> <channel>: forced part."
  (let* ((target (find-client (car params)))
         (chan-name (sanitize-string (cadr params) *max-channel-length*))
         (chan (and chan-name (find-channel chan-name))))
    (cond
      ((null target)
       (send-no-such-nick client (car params)))
      ((null chan)
       (send-no-such-channel client chan-name))
      ((not (on-channel-p target chan))
       (send-not-on-channel client chan-name "They aren't on that channel"))
      (t
       (part-from-channel target chan chan-name "Forced part")
       (log-security (format nil "SAPART ~A from ~A by ~A"
                             (client-nick target) chan-name nick))))))

(define-oper-command handle-samode "SAMODE" 2
  "SAMODE <channel> <modes> [args]: channel MODE that bypasses the
channel-operator requirement, reusing APPLY-CHANNEL-MODES."
  (let* ((target (car params))
         (chan (find-channel target)))
    (if (null chan)
        (send-no-such-channel client target)
        (progn
          (apply-channel-modes client chan target nick (cadr params) (cddr params))
          (log-security (format nil "SAMODE ~A ~{~A~^ ~} by ~A"
                                target (cdr params) nick))))))

(defun handle-command (client cmd params)
  "Dispatch CMD to its handler. Pre-registration only NICK/USER/PING/CAP/QUIT
are accepted; all others are silently dropped until the client is registered."
  (cond
    ((string= cmd "NICK")  (handle-nick  client params))
    ((string= cmd "USER")  (handle-user  client params))
    ((string= cmd "PING")  (handle-ping  client params))
    ((string= cmd "CAP")   (handle-cap   client params))
    ((string= cmd "QUIT")  (handle-quit  client params))
    ((client-registered client)
     (cond
       ((string= cmd "JOIN")    (handle-join    client params))
       ((string= cmd "PART")    (handle-part    client params))
       ((string= cmd "KICK")    (handle-kick    client params))
       ((string= cmd "INVITE")  (handle-invite  client params))
       ((string= cmd "PRIVMSG") (handle-privmsg client params))
       ((string= cmd "MODE")    (handle-mode    client params))
       ((string= cmd "TOPIC")   (handle-topic   client params))
       ((string= cmd "WHOIS")   (handle-whois   client params))
       ((string= cmd "WHO")     (handle-who     client params))
       ((string= cmd "NAMES")   (handle-names   client params))
       ((string= cmd "OPER")    (handle-oper    client params))
       ((string= cmd "ACCEPT")  (handle-accept  client params))
       ((string= cmd "KILL")    (handle-kill    client params))
       ((string= cmd "ZLINE")   (handle-zline   client params))
       ((string= cmd "GAG")     (handle-gag     client params))
       ((string= cmd "SAJOIN")  (handle-sajoin  client params))
       ((string= cmd "SAPART")  (handle-sapart  client params))
       ((string= cmd "SAMODE")  (handle-samode  client params))
       ;; Unknown command: reply 421 so clients fail fast instead of
       ;; blocking on a reply that never comes (the WHO-sync hang class).
       (t
        (send-error client *err-unknowncommand*
                    (list (client-nick* client) cmd "Unknown command")))))))

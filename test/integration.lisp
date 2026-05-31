;;;; Live-socket integration smoke test.  Run standalone:
;;;;   sbcl --non-interactive --load test/integration.lisp
;;;; Starts a server on a spare port, drives it with real sb-bsd-sockets
;;;; clients, and exits non-zero on any failed assertion.  Not part of the
;;;; ASDF unit suite (that one is allocation/parser focused and needs no I/O).
(require :asdf)
(asdf:load-asd (merge-pathnames "../sbcl-ircd.asd" *load-pathname*))
(asdf:load-system :sbcl-ircd)
(in-package :sbcl-ircd)
(load (merge-pathnames "smoke-common.lisp" *load-pathname*))

(setf *enable-logging* nil
      *smoke-port* 6699)

(with-smoke-checks (fails)
    (start-server :port *smoke-port* :host "127.0.0.1")
    (sleep 0.3)
    (let ((a (smoke-connect)) (b (smoke-connect)) (c (smoke-connect)))
      ;; --- Pre-registration ---
      (smoke-send a "NICK") (check "NICK no params -> 461" (smoke-has-numeric (smoke-drain a) "461"))
      (smoke-send a "NICK alice") (smoke-send a "USER alice 0 * :Alice")
      (let ((r (smoke-drain a)))
        (check "alice 001 in command field" (smoke-has-numeric r "001"))
        (check "registration advertises 005 ISUPPORT" (smoke-has-numeric r "005"))
        (check "registration ends with 376 (ENDOFMOTD)" (smoke-has-numeric r "376")))
      (smoke-send b "NICK bob") (smoke-send b "USER bob 0 * :Bob")
      (check "bob 001" (smoke-has-numeric (smoke-drain b) "001"))
      (smoke-send c "NICK carol") (smoke-send c "USER carol 0 * :Carol")
      (check "carol 001" (smoke-has-numeric (smoke-drain c) "001"))
      ;; --- WHOIS ---
      (smoke-send a "WHOIS") (check "WHOIS no params -> 461" (smoke-has-numeric (smoke-drain a) "461"))
      (smoke-send a "WHOIS alice") (check "WHOIS works" (smoke-has-numeric (smoke-drain a) "311"))
      ;; --- Channel join: RFC 2812 sends the topic ONLY if set; no 331 on join ---
      (smoke-send a "JOIN #room")
      (let ((r (smoke-drain a)))
        (check "JOIN with no topic sends no 331/332 (RFC)"
               (and (not (smoke-has-numeric r "331")) (not (smoke-has-numeric r "332"))))
        (check "JOIN still sends 353 names" (smoke-has-numeric r "353")))
      (smoke-send b "JOIN #room") (smoke-drain b) (smoke-drain a)
      (smoke-send a "PRIVMSG #room :hi bob")
      (check "channel PRIVMSG delivered" (smoke-has (smoke-drain b) "hi bob"))
      ;; --- TOPIC: set it, then a fresh joiner gets 332 + 333 (who/when) ---
      (smoke-send a "TOPIC #room :hello world") (smoke-drain a) (smoke-drain b)
      (smoke-send a "TOPIC #room")
      (let ((r (smoke-drain a)))
        (check "TOPIC query returns 332" (smoke-has-numeric r "332"))
        (check "TOPIC query returns 333 (set-by/when)" (smoke-has-numeric r "333")))
      (let ((d (smoke-connect)))
        (smoke-send d "NICK dave") (smoke-send d "USER dave 0 * :Dave") (smoke-drain d)
        (smoke-send d "JOIN #room")
        (let ((r (smoke-drain d)))
          (check "JOIN with topic set sends 332" (smoke-has-numeric r "332"))
          (check "JOIN with topic set sends 333" (smoke-has-numeric r "333")))
        (close d))
      ;; --- WHO (irssi join-sync: must always terminate with 315) ---
      (smoke-send a "WHO #room")
      (let ((r (smoke-drain a)))
        (check "WHO returns 352 reply for member" (smoke-has-numeric r "352"))
        (check "WHO terminates with 315 (ENDOFWHO)" (smoke-has-numeric r "315")))
      ;; --- Unknown command ---
      (smoke-send a "FROBNICATE foo")
      (check "unknown command returns 421" (smoke-has-numeric (smoke-drain a) "421"))
      ;; --- MODE query sends 324 + 329 (irssi sync gate) ---
      (smoke-send a "MODE #room")
      (let ((r (smoke-drain a)))
        (check "MODE query returns 324 (RPL_CHANNELMODEIS)" (smoke-has-numeric r "324"))
        (check "MODE query returns 329 (RPL_CREATIONTIME) after 324"
               (smoke-has-numeric r "329")))
      ;; --- Mode: non-op list query ---
      (smoke-send b "MODE #room b")
      (let ((r (smoke-drain b)))
        (check "non-op MODE #room b returns 368, not 482"
               (and (smoke-has-numeric r "368") (not (smoke-has-numeric r "482")))))
      ;; --- Mode: flag changes and query ---
      (smoke-send a "MODE #room +n") (smoke-drain a) (smoke-drain b)
      (smoke-send a "MODE #room -t") (smoke-drain a) (smoke-drain b)
      (smoke-send a "MODE #room")
      (let ((r (smoke-drain a)))
        (check "MODE query shows +n, not +t"
               (some (lambda (l)
                       (and (smoke-numeric-p l "324")
                            (let ((plus (search "+" l)))
                              (and plus
                                   (search "n" l :start2 plus)
                                   (not (search "t" l :start2 plus))))))
                     r)))
      ;; --- Per-channel mode isolation ---
      (smoke-send b "JOIN #other") (smoke-drain b)
      (smoke-send b "MODE #other")
      (check "#other still +t (no shared-literal corruption)"
             (some (lambda (l)
                     (and (smoke-numeric-p l "324") (search "t" l)))
                   (smoke-drain b)))
      ;; --- PART and mode guard ---
      (smoke-send a "MODE #room +o bob") (smoke-drain a) (smoke-drain b)
      (smoke-send b "PART #room") (smoke-drain b) (smoke-drain a)
      (smoke-send b "MODE #room +n")
      (check "PARTed user cannot change modes (442)"
             (smoke-has-numeric (smoke-drain b) "442"))
      ;; --- QUIT deduplication ---
      (smoke-send b "JOIN #room") (smoke-drain b) (smoke-drain a)
      (smoke-send b "JOIN #other") (smoke-drain b)
      (smoke-send c "JOIN #room") (smoke-drain c) (smoke-drain a) (smoke-drain b)
      (smoke-send c "JOIN #other") (smoke-drain c) (smoke-drain b)
      (smoke-drain c)
      (close b)
      (let ((r (smoke-drain c 700)))
        (check "carol sees bob QUIT" (smoke-has r "QUIT"))
        (check "carol sees exactly ONE QUIT (deduped across 2 shared channels)"
               (= 1 (smoke-count r "QUIT"))))
      (sleep 0.2)
      (check "bob nick unregistered after disconnect" (null (find-client "bob")))
      (close a) (close c))
    (stop-server)
    (sleep 0.2)
    (smoke-finish fails "INTEGRATION"))

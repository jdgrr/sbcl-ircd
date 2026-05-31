;;;; Live-socket integration smoke test for the operator-services layer:
;;;;   OPER, umodes +o/+x (host cloak)/+g (caller-id) + ACCEPT, GAG, KILL,
;;;;   ZLINE, SAJOIN, SAPART, SAMODE.  Run standalone:
;;;;   sbcl --non-interactive --load test/oper-integration.lisp
;;;; Drives the server with real sockets and exits non-zero on any failure.
(require :asdf)
(asdf:load-asd (merge-pathnames "../sbcl-ircd.asd" *load-pathname*))
(asdf:load-system :sbcl-ircd)
(in-package :sbcl-ircd)
(load (merge-pathnames "smoke-common.lisp" *load-pathname*))

(setf *enable-logging* nil
      *oper-credentials* '(("god" . "secret"))
      *message-rate-limit* 1000
      *smoke-port* 6698)

(with-smoke-checks (fails)
    (start-server :port *smoke-port* :host "127.0.0.1")
    (sleep 0.3)
    (let ((al (smoke-connect)) (bo (smoke-connect)) (op (smoke-connect)))
      (smoke-register al "alice")
      (let ((rb (smoke-register bo "bob")))
        (check "registration announces the +x cloak umode via MODE"
               (some (lambda (l) (and (search "MODE" l) (search "+x" l))) rb)))
      (smoke-register op "oper")

      ;; --- OPER ---
      (smoke-send op "OPER god wrongpass")
      (check "OPER bad password -> 464" (smoke-numeric-line (smoke-drain op) "464"))
      (smoke-send op "OPER god secret")
      (let ((r (smoke-drain op)))
        (check "OPER success -> 381" (smoke-numeric-line r "381"))
        (check "OPER echoes umode +o" (smoke-has r "MODE")))

      ;; --- Auto-cloak on connect: even a regular user is masked, and the real
      ;;     loopback IP never appears in the host field. ---
      (smoke-send al "WHOIS bob")
      (let ((line (smoke-numeric-line (smoke-drain al) "311")))
        (check "regular user is auto-cloaked in WHOIS (.ip cloak)"
               (and line (search ".ip" line)))
        (check "real IP 127.0.0.1 is never leaked"
               (and line (not (search "127.0.0.1" line)))))

      ;; --- non-oper cannot use oper commands -> 481 ---
      (smoke-send bo "KILL alice :nope")
      (check "non-oper KILL -> 481" (smoke-numeric-line (smoke-drain bo) "481"))

      ;; --- SAJOIN / SAPART ---
      (smoke-send op "SAJOIN bob #forced")
      (check "SAJOIN forces bob into #forced" (smoke-has (smoke-drain bo) "#forced"))
      (smoke-send op "SAPART bob #forced")
      (check "SAPART forces bob out of #forced"
             (let ((r (smoke-drain bo))) (and (smoke-has r "PART") (smoke-has r "#forced"))))

      ;; --- SAMODE: oper not on channel grants +o bypassing chanop check ---
      (smoke-send bo "JOIN #samode") (smoke-drain bo)
      (smoke-send op "SAMODE #samode -o bob") (smoke-drain bo)
      (smoke-send op "SAMODE #samode +o bob")
      (check "SAMODE +o delivered to bob"
             (let ((r (smoke-drain bo))) (and (smoke-has r "MODE") (smoke-has r "+o"))))

      ;; --- Ban matches the visible cloak: a chanop bans the cloaked host it
      ;;     actually sees, and the rejoin is blocked (474). ---
      (smoke-send al "JOIN #bantest") (smoke-drain al)              ; alice joins first -> chanop
      (smoke-send bo "JOIN #bantest")
      (let* ((jline (find-if (lambda (l) (search "JOIN" l)) (smoke-drain al)))
             (prefix (and jline (first (parse-irc-message jline))))
             (host (and prefix (find #\@ prefix)
                        (subseq prefix (1+ (position #\@ prefix))))))
        (check "alice sees bob's cloaked JOIN prefix"
               (and host (search ".ip" host) (not (search "127.0.0.1" host))))
        (smoke-send al (format nil "MODE #bantest +b *!*@~A" host)) (smoke-drain al) (smoke-drain bo)
        (smoke-send bo "PART #bantest") (smoke-drain bo) (smoke-drain al)
        (smoke-send bo "JOIN #bantest")
        (check "ban on the cloaked host blocks rejoin (474)"
               (smoke-numeric-line (smoke-drain bo) "474")))

      ;; --- GAG: gagged user's channel message is silently dropped ---
      (smoke-send bo "JOIN #gag") (smoke-send op "SAJOIN oper #gag") (smoke-drain bo) (smoke-drain op)
      (smoke-send op "GAG bob") (smoke-drain op) (smoke-drain bo)
      (smoke-send bo "PRIVMSG #gag :GAGGEDTEXT")
      (check "gagged bob's message is dropped" (not (smoke-has (smoke-drain op) "GAGGEDTEXT")))
      (check "gagged bob receives no error" (null (smoke-drain bo 150)))
      (smoke-send op "GAG bob OFF") (smoke-drain op)
      (smoke-send bo "PRIVMSG #gag :UNGAGTEXT")
      (check "ungagged bob's message flows again" (smoke-has (smoke-drain op) "UNGAGTEXT"))

      ;; --- +g caller-id + ACCEPT ---
      (smoke-send al "MODE alice +g") (smoke-drain al)
      (smoke-send bo "PRIVMSG alice :BLOCKEDMSG")
      (check "sender of +g target gets 716" (smoke-numeric-line (smoke-drain bo) "716"))
      (let ((r (smoke-drain al)))
        (check "+g target gets 718 notify" (smoke-numeric-line r "718"))
        (check "+g blocks the actual message" (not (smoke-has r "BLOCKEDMSG"))))
      (smoke-send op "PRIVMSG alice :OPERBYPASS")
      (check "operator bypasses +g" (smoke-has (smoke-drain al) "OPERBYPASS"))
      (smoke-send al "ACCEPT bob") (smoke-drain al)
      (smoke-send bo "PRIVMSG alice :ALLOWEDMSG")
      (check "ACCEPTed sender reaches +g target" (smoke-has (smoke-drain al) "ALLOWEDMSG"))

      ;; --- KILL: forced disconnect, witnessed QUIT carries the reason ---
      (smoke-send al "JOIN #room") (smoke-send bo "JOIN #room") (smoke-drain al) (smoke-drain bo)
      (smoke-send op "KILL alice :badbehaviour")
      (let ((r (smoke-drain bo 500)))
        (check "channel peer sees KILLed user QUIT" (smoke-has r "QUIT"))
        (check "QUIT reason reflects KILL" (smoke-has r "Killed by")))
      (sleep 0.2)
      (check "killed client is unregistered" (null (find-client "alice")))

      ;; --- ZLINE (last: it kills every loopback client, op included) ---
      (smoke-send op "ZLINE 127.0.0.1 :go away")
      (sleep 0.2)
      (check "ZLINE disconnects matching connected client" (null (find-client "bob")))
      (let ((late (smoke-connect)))
        (smoke-send late "NICK late") (smoke-send late "USER late 0 * :late")
        (check "ZLINE refuses new matching connection"
               (null (smoke-numeric-line (smoke-drain late 400) "001")))
        (ignore-errors (close late)))
      (ignore-errors (close al)) (ignore-errors (close bo)) (ignore-errors (close op)))
    (stop-server)
    (sleep 0.2)
    (smoke-finish fails "OPER"))

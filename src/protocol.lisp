(in-package #:sbcl-ircd)

;; IRC numeric reply constants (RFC 1459/2812).  Only those actually used in
;; this codebase are defined; literals stay literals when used exactly once.
(defparameter *rpl-welcome*           "001")
(defparameter *rpl-yourhost*          "002")
(defparameter *rpl-created*           "003")
(defparameter *rpl-myinfo*            "004")
(defparameter *rpl-isupport*          "005")
(defparameter *rpl-umodeis*           "221")
(defparameter *rpl-acceptlist*        "281")
(defparameter *rpl-endofaccept*       "282")
(defparameter *rpl-youreoper*         "381")
(defparameter *rpl-whoisuser*         "311")
(defparameter *rpl-whoisserver*       "312")
(defparameter *rpl-endofwhois*        "318")
(defparameter *rpl-channelmodeis*     "324")
(defparameter *rpl-creationtime*      "329")
(defparameter *rpl-inviting*          "341")
(defparameter *rpl-invitelist*        "346")
(defparameter *rpl-endofinvitelist*   "347")
(defparameter *rpl-notopic*           "331")
(defparameter *rpl-topic*             "332")
(defparameter *rpl-topicwhotime*      "333")
(defparameter *rpl-whoreply*          "352")
(defparameter *rpl-endofwho*          "315")
(defparameter *rpl-namreply*          "353")
(defparameter *rpl-endofnames*        "366")
(defparameter *rpl-banlist*           "367")
(defparameter *rpl-endofbanlist*      "368")
(defparameter *rpl-exceptlist*        "348")
(defparameter *rpl-endofexceptlist*   "349")
(defparameter *rpl-motd*              "372")
(defparameter *rpl-motdstart*         "375")
(defparameter *rpl-endofmotd*         "376")
;; Caller-id (+g) notifications, RFC-less but de-facto (charybdis/ratbox).
(defparameter *rpl-targumodeg*        "716")
(defparameter *rpl-targnotify*        "717")
(defparameter *rpl-umodegmsg*         "718")

(defparameter *err-nosuchnick*        "401")
(defparameter *err-nosuchchannel*     "403")
(defparameter *err-cannotsendtochan*  "404")
(defparameter *err-toomanychannels*   "405")
(defparameter *err-norecipient*       "411")
(defparameter *err-notexttosend*      "412")
(defparameter *err-erroneousnickname* "432")
(defparameter *err-nicknameinuse*     "433")
(defparameter *err-notonchannel*      "442")
(defparameter *err-unknowncommand*    "421")
(defparameter *err-needmoreparams*    "461")
(defparameter *err-alreadyregistered* "462")
(defparameter *err-passwdmismatch*    "464")
(defparameter *err-noprivileges*      "481")
(defparameter *err-inviteonlychan*    "473")
(defparameter *err-bannedfromchan*    "474")
(defparameter *err-chanoprisneeded*   "482")
(defparameter *err-usersdontmatch*    "502")

(defun parse-irc-message (line)
  "Parses a raw IRC line into a list of (prefix command parameters).
   Uses index-based parsing to minimize string allocations."
  (declare (type simple-string line)
           (optimize (speed 3) (safety 1)))
  (let* ((len (length line))
         (start 0)
         (end len))
    ;; Trim trailing CR/LF/space in-place via index adjustment
    (loop while (and (> end start)
                     (let ((ch (char line (1- end))))
                       (or (char= ch #\Return) (char= ch #\Newline) (char= ch #\Space))))
          do (decf end))
    ;; Trim leading spaces
    (loop while (and (< start end) (char= (char line start) #\Space))
          do (incf start))
    (when (>= start end)
      (return-from parse-irc-message nil))
    (let (prefix command params
          (pos start))
      ;; 1. Extract prefix (if present)
      (when (char= (char line pos) #\:)
        (let ((space-pos (position #\Space line :start (1+ pos) :end end)))
          (if space-pos
              (progn
                (setf prefix (subseq line (1+ pos) space-pos))
                (setf pos (1+ space-pos)))
              (return-from parse-irc-message nil))))
      ;; Skip spaces
      (loop while (and (< pos end) (char= (char line pos) #\Space))
            do (incf pos))
      ;; 2. Extract command (uppercase in-place)
      (when (< pos end)
        (let ((space-pos (position #\Space line :start pos :end end)))
          (let ((cmd-end (or space-pos end)))
            (setf command (nstring-upcase (subseq line pos cmd-end)))
            (setf pos (if space-pos (1+ space-pos) end)))))
      ;; 3. Extract parameters
      (loop while (< pos end) do
        ;; Skip spaces
        (loop while (and (< pos end) (char= (char line pos) #\Space))
              do (incf pos))
        (when (< pos end)
          (if (char= (char line pos) #\:)
              ;; Trailing parameter - rest of line
              (progn
                (push (subseq line (1+ pos) end) params)
                (setf pos end))
              ;; Normal parameter
              (let ((space-pos (position #\Space line :start pos :end end)))
                (if space-pos
                    (progn
                      (push (subseq line pos space-pos) params)
                      (setf pos (1+ space-pos)))
                    (progn
                      (push (subseq line pos end) params)
                      (setf pos end)))))))
      (list prefix command (nreverse params)))))

(defun format-irc-message (prefix command params &key (force-trailing t))
  "Format an IRC message.  When FORCE-TRAILING, the last parameter is always
written as the ':' trailing argument."
  (with-output-to-string (str)
    (when prefix
      (write-char #\: str)
      (write-string prefix str)
      (write-char #\Space str))
    (write-string command str)
    (when params
      (let ((last-idx (1- (length params))))
        (loop for idx from 0 for param in params do
          (write-char #\Space str)
          (when (and (= idx last-idx)
                     (or force-trailing
                         (string= param "")
                         (position #\Space param)
                         (and (> (length param) 0) (char= (char param 0) #\:))))
            (write-char #\: str))
          (write-string param str))))
    (write-char #\Return str)
    (write-char #\Newline str)))

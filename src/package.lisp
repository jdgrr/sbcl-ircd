(defpackage #:sbcl-ircd
  (:use #:cl #:sb-bsd-sockets #:sb-thread)
  ;; Main server API
  (:export #:start-server
           #:stop-server
           #:*server-port*
           #:*server-host*
           #:*default-port*
           #:*default-host*
           #:main)
  ;; Logging functions
  (:export #:log-info
           #:log-warning
           #:log-error
           #:log-security)
  ;; Server identity (must be configured before start-server)
  (:export #:*server-name*
           #:*server-version*
           #:*creation-date*
           #:*motd*)
  ;; Server state and configuration
  (:export #:*max-connections*
           #:*max-message-length*
           #:*max-nick-length*
           #:*max-channel-length*
           #:*max-channels-per-client*
           #:*connection-timeout*
           #:*message-rate-limit*
           #:*connection-rate-limit*
           #:*enable-logging*
           #:*log-file*
           #:*oper-credentials*)
  ;; Client accessors
  (:export #:client-socket
           #:client-stream
           #:client-host
           #:client-nick
           #:client-user
           #:client-realname
           #:client-registered
           #:client-channels
           #:client-thread
           #:client-last-activity
           #:client-message-count
           #:client-message-window-start
           #:client-lock
           #:client-message-queue)
  ;; Channel accessors
  (:export #:channel-name
           #:channel-topic
           #:channel-topic-setter
           #:channel-topic-time
           #:channel-operators
           #:channel-voiced
           #:channel-bans
           #:channel-ban-exceptions
           #:channel-invites
           #:channel-invite-exceptions
           #:channel-modes
           #:channel-creation-time
           #:channel-client-set
           #:channel-lock)
  ;; State management functions
  (:export #:find-client
           #:find-channel
           #:get-or-create-channel
           #:increment-connection-count
           #:decrement-connection-count
           #:check-connection-limit
           #:check-ip-rate-limit
           #:check-client-message-rate
           #:update-client-activity
           #:client-idle-p
           #:add-active-client
           #:remove-active-client
           #:active-clients)
  ;; Validation functions
  (:export #:valid-nick-p
           #:valid-channel-name-p
           #:sanitize-string)
  ;; IRC protocol functions
  (:export #:parse-irc-message
           #:format-irc-message))

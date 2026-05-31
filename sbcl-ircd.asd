;;;; sbcl-ircd.asd

(asdf:defsystem #:sbcl-ircd
  :description "A simple IRC daemon implementation in Common Lisp using SBCL"
  :author "SBCL-IRCD Contributors"
  :license "MIT"
  :version "1.2"
  :serial t
  :depends-on (#:sb-bsd-sockets
               #:sb-concurrency
               #:sb-md5)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "state")
                 (:file "protocol")
                 (:file "commands")
                 (:file "server")
                 (:file "main"))))
  :in-order-to ((test-op (test-op #:sbcl-ircd/test))))

(asdf:defsystem #:sbcl-ircd/test
  :description "Test system for sbcl-ircd"
  :author "SBCL-IRCD Contributors"
  :license "MIT"
  :depends-on (#:sbcl-ircd)
  :components ((:module "test"
                :components
                ((:file "tests"))))
  ;; RUN-TESTS lives in the SBCL-IRCD-TEST package (tests.lisp), not SBCL-IRCD.
  ;; UIOP:SYMBOL-CALL avoids a read-time package dependency; we ERROR on a
  ;; failed suite so ASDF:TEST-SYSTEM propagates a non-zero exit code to CI.
  :perform (test-op (op c)
                    (declare (ignore op c))
                    (unless (uiop:symbol-call '#:sbcl-ircd-test '#:run-tests)
                      (error "sbcl-ircd test suite failed"))))
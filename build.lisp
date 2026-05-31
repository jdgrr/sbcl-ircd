#!/bin/sh
#|
exec sbcl --script "$0" "$@"
|#
;;;; Build a standalone executable IRC daemon. Either invocation works:
;;;;   ./build.lisp              (self-executing script)
;;;;   sbcl --script build.lisp  (explicit)
;;;; NOT --load: the #! shebang line is a reader error unless --script skips it.
;;;; Both yield ./sbcl-ircd, started with optional PORT HOST args.
(require :asdf)
(asdf:load-asd (merge-pathnames "sbcl-ircd.asd" *load-pathname*))
(asdf:load-system :sbcl-ircd)

(sb-ext:save-lisp-and-die
 ;; :TYPE NIL so the name stays "sbcl-ircd" and doesn't inherit build.lisp's
 ;; ".lisp" type via the defaults.
 (make-pathname :name "sbcl-ircd" :type nil :defaults *load-pathname*)
 :executable t
 :toplevel (lambda () (sbcl-ircd:main))
 :save-runtime-options t
 #+sb-core-compression :compression #+sb-core-compression t)

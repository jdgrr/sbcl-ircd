#!/usr/bin/env sbcl --script
(require :asdf)
(asdf:load-asd (merge-pathnames "sbcl-ircd.asd" *load-pathname*))
(asdf:load-system :sbcl-ircd)
(sbcl-ircd:main)


(ql:quickload :sup)

(load "sup.lisp")

(ccl:save-application "sup.world"
                      :purify t
                      :init-file "init.lisp"
                      :prepend-kernel t)


(in-package :cl-user)

(load-all-patches)

(load "~/quicklisp/setup.lisp")

(ql:quickload :sup)

(deliver 'sup:interface-main
         (create-macos-application-bundle
          "Sup.app")
         0
         :interface :capi)

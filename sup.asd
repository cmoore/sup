
(asdf:defsystem #:sup
  :description "Describe sup here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:json-mop
               :drakma
               :uuid
               :easy-routes
               :parenscript
               :css-lite
               :cl-who
               :split-sequence
               :cl-mango
               :cl-ivy)
  :components ((:file "sup")))

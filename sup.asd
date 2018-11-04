
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
               :mailbox
               :split-sequence
               :cl-mango)
  :components ((:file "sup")))


(asdf:defsystem #:sup
  :description "Describe sup here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:json-mop
               :drakma
               :uuid
               :cl-ivy
               :local-time
               :html-entities
               :hunchentoot
               :hunchensocket
               :easy-routes
               :parenscript
               :css-lite
               :spinneret
               :spinneret/cl-markdown
               :split-sequence
               :cl-hash-util
               :safe-queue
               :mailbox

               :cl-postgres+local-time
               :postmodern)
  
  :components ((:file "sup")))

;;;; cl-combinator.asd

(asdf:defsystem #:cl-combinator
  :description "Describe cl-combinator here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:fset)
  :components ((:file "package")
               (:file "utils")
               (:file "cl-combinator")
               (:file "parser")))

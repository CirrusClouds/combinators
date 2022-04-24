;;;; package.lisp

(defpackage #:cl-combinator
  (:use #:cl)
  (:nicknames :p)
  (:local-nicknames (#:f #:fset))
  (:shadow :char :block :many :choice :null :1+ :variable :word :number :float :function :ratio :var))

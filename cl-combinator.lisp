;;;; cl-combinator.lisp

(in-package #:cl-combinator)

(defun state-p (x)
  (or (equal x 'Success)
      (equal x 'Failure)))

(deftype state ()
  `(and symbol
    (satisfies state-p)))

(defstruct result
  (state 'Failure :type state)
  (result '() :type list)
  (remaining '() :type list))

(defun read-in ()
  (make-result :remaining (coerce (read-line) 'list)))


;;; Parse utils

(defun pchar (char)
  "Parse a single character"
  (let ((char (coerce char 'character)))
    (lambda (input)
      (if (equal char (car (result-remaining input))) 
          (make-result :state 'Success
                       :result (append (result-result input) (list char))
                       :remaining (cdr (result-remaining input)))
          (make-result :state 'Failure :result (result-result input) :remaining (result-remaining input))
          ))))

(defun listparse (parsers)
  "Parse multiple in a row and concatenate"
  (lambda (input)
    (cond ((null parsers)
           input)
          (t
           (let ((output (funcall (car parsers) input)))
             (if (equal (result-state output) 'Success)
                 (funcall (listparse (cdr parsers)) output)
                 output))))))

(defun parsechoice (parsers)
  "Parse one of"
  (lambda (input)
    (cond ((null parsers)
           input)
          (t
           (let ((output (funcall (car parsers) input)))
             (if (equal (result-state output) 'Success)
                 output
                 (funcall (parsechoice (cdr parsers)) output)))))))

(defun manyparse (parser &optional (count 0))
  "Parse the same repeatedly as many times as possible"
  (lambda (input)
    (let ((output (funcall parser input)))
      (if (equal (result-state output) 'Success)
          (funcall (manyparse parser) output)
          (if (= count 0)
              input
              (make-result :state 'Success
                           :result (result-result output)
                           :remaining (result-remaining output)))))))


(defun seqparse (parsers)
  (lambda (input)
    (let* ((clean-input (make-result :state 'Failure
                                     :result nil
                                     :remaining (result-remaining input)))
           (output (funcall (listparse parsers) clean-input)))
      (if (equal (result-state output) 'Success)
          (make-result :state 'Success
                       :result (append (result-result input) (list (result-result output)))
                       :remaining (result-remaining output))
          input))))


(defun any-of-parse (str)
  (lambda (input)
    (funcall (parsechoice (mapcar #'pchar (coerce str 'list))) input)))


(defun 0-or-more-parse (parser)
  (lambda (input)
    (let ((output (funcall parser input)))
      (if (equal (result-state output) 'Failure)
          (make-result :state 'Success :result (result-result input) :remaining (result-remaining input))
          (funcall (manyparse parser) input)))))


(defun surrounded-by-parse (a b c)
  (lambda (input)
    (let ((aresult (funcall a input)))
      (if (equal (result-state aresult) 'Success)
          (let ((bresult (funcall b aresult)))
            (if (equal (result-state bresult) 'Success)
                (let ((cresult (funcall c bresult)))
                  (if (equal (result-state cresult) 'Success)
                      (progn
                        (make-result :state 'Success
                                     :result (result-result bresult)
                                     :remaining (result-remaining cresult)))
                      cresult))
                bresult))
          aresult))))

(defun parse-no-result (parser)
  (lambda (input)
    (let ((output (funcall parser input)))
      (make-result :state (result-state output)
                   :result (result-result input)
                   :remaining (result-remaining output)))))

(defun surrounded-by-parse (a b c)
  (lambda (input)
    (let ((aresult (funcall (parse-no-result a) input)))
      (if (equal (result-state aresult) 'Success)
          (let ((bresult (funcall b aresult)))
            (if (equal (result-state bresult) 'Success)
                (let ((cresult (funcall (parse-no-result c) bresult)))
                  (if (equal (result-state cresult) 'Success)
                      (make-result :state 'Success
                                   :result (result-result bresult)
                                   :remaining (result-remaining cresult))
                      cresult))
                bresult))
          aresult))))


;;; Atomic Parsers

(setf *char-parser* (any-of-parse "abcdefghijklmnopqrstuvwxyz"))

(setf *digit-parser* (any-of-parse "1234567890"))

(setf *op-parser* (any-of-parse "+-/*="))

(defun number-parse ()
  (lambda (input)
    (funcall (seqparse (list (manyparse *digit-parser*))) input)))

(defun interpret-number ()
  (lambda (input)
    (let ((output (funcall (number-parse) input)))
      (if (equal (result-state output) 'Success)
          (make-result :state 'Success
                       :result (append (result-result input) (list (parse-integer (coerce (first (last (result-result output))) 'string))))
                       :remaining (result-remaining output))
          input))))

(defun map-to-op (str)
  (cond ((string= str "+") '+)
        ((string=  str "-") '-)
        ((string= str "*") '*)
        ((string= str "/") '/)
        ((string= str "=") '=)
        (t
         (error "~A doesn't match any operator" str))))


(defun op-parse ()
  (lambda (input)
    (let ((output (funcall (seqparse (list *op-parser*)) input)))
      (if (equal (result-state output) 'Success)
          (make-result :state 'Success
                       :result (append (result-result input) (list (map-to-op (coerce (first (last (result-result output))) 'string))))
                       :remaining (result-remaining output))
          input))))

(defun sym-parse ()
  (lambda (input)
    (funcall (manyparse (parsechoice (list *digit-parser* *char-parser*))) input)))

(defun interpret-sym ()
  (lambda (input)
    (let ((output (funcall (seqparse (list (sym-parse))) input)))
      (if (equal (result-state output) 'Success)
          (make-result :state 'Success
                       :result (append (result-result input) (list (read-from-string (coerce (first (last (result-result output))) 'string))))
                       :remaining (result-remaining output))
          input))))

;; (defun empty-list-parse ()
;;   (lambda (input)
;;     (let ((output (funcall (seqparse (list (pchar "(")
;;                                            (pchar ")"))) input)))
;;       (if (equal (result-state output) 'Success)
;;           (make-result :state 'Success
;;                        :result (append (result-result input) (list (read-from-string (coerce (first (last (result-result output))) s 'string))))
;;                        :remaining (result-remaining output))
;;           input))))

(defun null-parse ()
  (lambda (input)
    (make-result :state (result-state input)
                 :result (result-result input)
                 :remaining (result-remaining input))))


;;; S-Expressions

(defun sexpr-parse ()
  (lambda (input)
    (funcall
     (surrounded-by-parse (pchar "(")
                          (seqparse (list
                                     (parsechoice (list (interpret-sym) (op-parse)))
                                     (manyparse (listparse (list
                                                            (parse-no-result (pchar " "))
                                                            (parsechoice (list
                                                                          (sexpr-parse)
                                                                          (empty-list-parse)
                                                                          (interpret-sym)
                                                                          (interpret-number))))))))
                          (pchar ")")) input)))


(defun lispy-parse ()
  (lambda (input)
    (let ((output
            (funcall (sexpr-parse) input)))
      (make-result :state (result-state output)
                   :result (car (result-result output))
                   :remaining (result-remaining output)))))


;;; Deep interpreter water

(defun eval-op (op)
  (cond
    ((macro-function op)
     (error "Macro ~A not yet implemented in MINILISP - sorry!" op))
    (t
     op)))

(defun traverse-AST (tree)
  (cond ((typep tree 'integer)
         tree)
        ((typep tree 'symbol)
         tree)
        ((listp tree)
         (let ((op (first tree)))
           (apply (eval-op op) (mapcar #'traverse-AST (cdr tree))))))) ;; For now we'll simply apply.

;;; The REPL!

(defun interpret-minilisp ()
  (terpri)
  (format t "MINILISP v0.0.1 || Press ??? to exit")
  (terpri)
  (terpri)
  (terpri)
  (loop :while t
        :do
           (format t "MINILISP> ")
           (format t "~A" (traverse-AST (result-result (funcall (lispy-parse) (read-in)))))
           (terpri)))

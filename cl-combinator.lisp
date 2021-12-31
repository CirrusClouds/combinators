;;;; cl-combinator.lisp

(in-package #:cl-combinator)

(defun state-p (x)
  (or (equal x 'Success)
      (equal x 'Failure)))

(deftype state ()
  '(and symbol
    (satisfies state-p)))

(defstruct result
  (state 'Failure :type state)
  (result '() :type list)
  (remaining '() :type list))

(defun read-in ()
  (make-result :remaining (coerce (read-line) 'list)))

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

(defun manyparse (parser)
  "Parse the same repeatedly as many times as possible"
  (lambda (input)
    (let ((output (funcall parser input)))
      (if (equal (result-state output) 'Success)
          (funcall (manyparse parser) output)
          input))))

(defun seqparse (parsers)
  "Return a list of results for each parse instead of concatenating"
  (lambda (input)
    (let ((acc '()))
      (reduce (lambda (in p2)
                (let ((output (funcall p2 in)))
                  (setf acc (append acc (list output)))
                  (make-result :state 'Failure :remaining (result-remaining output))))
              parsers :initial-value input)
      acc)))

(setf *char-parser* (parsechoice (list (pchar "a") (pchar "b") (pchar "c") (pchar "d") (pchar "e") (pchar "f") (pchar "g") (pchar "h") (pchar "i") (pchar "j") (pchar "k") (pchar "l") (pchar "m") (pchar "n") (pchar "o") (pchar "p") (pchar "q") (pchar "r") (pchar "s") (pchar "t") (pchar "u") (pchar "v") (pchar "w") (pchar "x") (pchar "y") (pchar "z"))))

(setf *digit-parser* (parsechoice (list (pchar "0") (pchar "1") (pchar "2") (pchar "3") (pchar "4") (pchar "5") (pchar "6") (pchar "7") (pchar "8") (pchar "9"))))

(setf *word-parser* (manyparse *char-parser*))

(setf *op-parser* (parsechoice (list (pchar "+") (pchar "-") (pchar "*") (pchar "/"))))

(setf *expr-parser* (seqparse (list
                               (pchar "(")
                               *op-parser*
                               (pchar " ")
                               *digit-parser*
                               (pchar " ")
                               *digit-parser*
                               (pchar ")"))))

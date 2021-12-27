;;;; cl-combinator.lisp

(in-package #:cl-combinator)

(defstruct result
  (state t)
  (value nil)
  remaining)

(defclass Parser ()
  ((parseFn
    :initarg :parseFn
    :accessor :parseFn)))


(defmethod run ((obj Parser) (str string))
  (funcall (slot-value obj 'parseFn) (make-result :remaining (coerce str 'list))))


;; Parse functions

(defun pchar (char)
  "Takes result struct, returns result struct. Builds up value"
  (lambda (ctx)
    (cond ((null (result-remaining ctx))
           (make-result :state nil
                        :value (result-value ctx)
                        :remaining (result-remaining ctx)))
          ((string= (car (result-remaining ctx)) char)
           (make-result :state t
                        :value (append (result-value ctx) (cons char nil))
                        :remaining (cdr (result-remaining ctx))))
          (t
           (make-result :state nil
                        :value (result-value ctx)
                        :remaining (result-remaining ctx))))))

(defun andparse (parser1 parser2)
  (lambda (ctx)
    (setf result1 (funcall parser1 ctx))
    (if (result-state result1)
        (funcall parser2 result1)
        result1)))

(defun multiparse (parsers)
  (lambda (ctx)
    (setf *result*
          (reduce (lambda (cur func2)
                    (if (result-state cur)
                        (funcall func2 cur)
                        cur))
                  parsers
                  :initial-value ctx))))

(defun wparse (charlist)
  (mapcar (lambda (c)
            (pchar c))
          charlist))

(defun succeeds (parser)
  (lambda (ctx)
    (setf *cur* (funcall parser ctx))
    (if (and (null (result-remaining *cur*))
             (result-state *cur*))
        t
        nil)
    ))

(defun parsemany (parser)
  (lambda (ctx)
    (setf *cur* (funcall parser ctx))
    (if (result-state *cur*)
        (funcall (parsemany parser) *cur*)
        ctx)))

(defun parsechoice (parsers)
  (lambda (ctx)
    (funcall (reduce (lambda (func1 func2)
                (if (result-state (funcall func1 ctx))
                    func1
                    func2))
              parsers) ctx)
    ))

(defun stringparser (_string)
  (lambda (ctx)
    (setf *liststring* (coerce _string 'list))
    (setf *parsexs* (wparse *liststring*))
    (funcall (multiparse *parsexs*) ctx)
    ))

(defun manydigitparser ()
  (lambda (ctx)
    (funcall (parsechoice (wparse (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))) ctx)))



(defun wordparser ()
  (lambda (ctx)
    (setf *result* (funcall (parsechoice (wparse (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))) ctx))
    (if (result-state *result*)
        (funcall (wordparser) *result*)
        *result*)))


(defun seqparse (parsers)
  (lambda (ctx)
    (setf *cur* ctx)
    (setf results '())
    (loop :for parser :in parsers
          :do
             (progn
               (setf *res* (funcall parser (make-result :remaining (result-remaining *cur*))))
               (setf results (append results (list (result-value *res*))))
               (setf *cur* *res*)))
    (make-result :value results :remaining (result-remaining *cur*))))

(defun hellothereparser ()
  (lambda (ctx)
    (funcall (seqparse (list (stringparser "hello")
                             (pchar " ")
                             (stringparser "three")))
             ctx)))

(defun parenparser ()
  (lambda (ctx)
    (funcall (seqparse (list (stringparser "(")
                             (wordparser)
                             (stringparser ")"))) ctx)))



;; Running and reading
(defun read-into-parser ()
  (setf *input* (coerce (read-line) 'list))
  (setf *ctx* (make-result :remaining *input*))
  *ctx*)

(defun run-parser (parser)
  (funcall parser (read-into-parser)))

(in-package #:cl-combinator)
;; Example of the combinators in action with a simple lisp to ada compiler
;; Useage: (-> "example4.txt" (parse) (build) (compile-ada-file))

(defun letter ()
  (with-state state
    (funcall (choice (f:image #'char *alpha-chars*))
             state)))


(defun digit ()
  (with-state state
    (funcall (choice (f:image #'char *digit-chars*)) state)))


(defun ws ()
  (with-state state
    (funcall (null (choice (f:seq (char #\ )
                                  (char #\linefeed )
                                  (char #\tab ))))
             state)))


(defun word ()
  (with-state state
    (funcall (1+ (letter))
             state)))


(defun number ()
  (with-state state
    (funcall (1+ (digit))
             state)))


(defun float ()
  (with-state state
    (funcall (many (f:seq
                    (number)
                    (char #\.)
                    (number)))
             state)))


(defun ratio ()
  (with-state state
    (funcall (many (f:seq
                    (number)
                    (char #\/)
                    (number)))
             state)))


(defun str ()
  (with-state state
    (funcall (many (f:seq (char #\" )
                          (0+ (except (f:seq #\" )))
                          (char #\" )))
             state)))

(defun var ()
  (with-state state
    (funcall (1+ (choice (f:seq (letter) (char #\_)))) state)))


(defun value ()
  "A value is a variable, a number, etc"
  (with-state-and-label state "value"
    (funcall (choice (f:seq (str) (ratio) (float) (var) (number)))
             state)))


(defun operation ()
  (with-state state
    (funcall (choice (f:seq (char #\+)
                            (char #\-)
                            (char #\*)
                            (char #\/)))
             state)))


(defun function ()
  "A function is either some word, or a defined operation"
  (with-state-and-label state "function"
    (funcall (choice (f:seq (operation)
                            (word)
                            (s-expr)))
             state)))


(defun s-expr ()
  (with-state state
    (funcall (many (f:seq
                    (null (char #\( ))
                    (block (? (function)))
                    (0+ (ws))
                    (0+ (block (expression)))
                    (null (char #\) ))
                    (0+ (ws))))
             state)))

(defun expression ()
  "Expression is either a value or a a function with arguments that are expressions"
  (with-state state
    (funcall (choice (f:seq
                      (s-expr)
                      (many (f:seq (value)
                                   (0+ (ws))))))
             state)))


(defun program ()
  "A program is a list of expressions :)
   also everything should be a lisp."
  (with-state-and-label state "program"
    (funcall (many (f:seq
                    (0+ (ws))
                    (until-consumed (block (many (f:seq (expression)
                                                        (0+ (ws))))))))
             state)))


(defun parse (s)
  (funcall (program) (read-in s)))

;;
;; Compilation
;; 


(defun build-func (function)
  (if (typep (f:first function) 'fset:seq)
      (build-sexpr function)
      (seq-to-string function)))


(defun build-arg (arg)
  (let ((arg-as-set (f:convert 'f:set arg)))
    (cond
      ((seq-to-list (f:intersection (f:set #\")
                                    arg-as-set))
       (f:map
        (:op "arg")
        (:type 'STRING)
        (:value (seq-to-string arg))))
      ((seq-to-list (f:intersection (f:set #\/)
                                    arg-as-set))
       (f:map
        (:op "arg")
        (:type 'RATIO)
        (:value (seq-to-string arg))))
      ((seq-to-list (f:intersection (f:set #\.)
                                    arg-as-set))
       (f:map
        (:op "arg")
        (:type 'FLOAT)
        (:value (seq-to-string arg))))
      ((seq-to-list (f:intersection (f:convert 'f:set *digit-chars*)
                                    arg-as-set))
       (f:map
        (:op "arg")
        (:type 'INT)
        (:value (seq-to-string arg))))
      ((seq-to-list (f:intersection (f:convert 'f:set *alpha-chars*)
                                    arg-as-set))
       (f:map
        (:op "arg")
        (:type 'VAR)
        (:value (seq-to-string arg))))
      (t
       (f:map                           ;lookup
        (:op "arg")
        (:type 'N/A)
        (:value (seq-to-string arg)))))))


(defun build-sexpr (expression)
  (f:map
   (:op "func")
   (:function (build-func (f:first expression)))
   (:args (f:image (lambda (arg)
                     (if (typep (f:first arg) 'fset:seq)
                         (build-sexpr arg)
                         (build-arg arg)))
                   (f:less-first expression)))))


(defun build-expr (expression)
  (if (typep (f:first expression) 'f:seq)
      (build-sexpr expression)
      (build-arg expression)))


(defun build (parsed-program)
  (if (equalp (f:@ parsed-program :state) 'FAILURE)
      (format t "Parsing Error: ~s~%Line: ~s~%Position: ~s~%Full Traceback:~%~{ ~s~%~}"
              (f:first (f:@ parsed-program :errors))
              (f:@ parsed-program :line)
              (f:@ parsed-program :position)
              (seq-to-list (f:@ parsed-program :labels)))
      (f:image #'build-expr (f:@ parsed-program :result))))




(defun addition-compile (addition-operation)
  (let ((operands (seq-to-list addition-operation)))
    (format nil "~{~A ~^+ ~}" operands)))

(defun dot-compile (dot-operation)
  (let ((operands (seq-to-list dot-operation)))
    (format nil "~{~A~^.~}" operands)))

(defun list-compile (list-operation)
  (let ((operands (seq-to-list list-operation)))
    (format nil "( ~{~A~^, ~} )" operands)))

(defun arglist-compile (list-operation)
  (let ((operands (seq-to-list list-operation)))
    (format nil "( ~{~a : ~a~^, ~} )" operands)))

(defun compile-line (line)
  (cond
    ((string= (f:@ line :op) "arg")
     (f:@ line :value))
    ((typep (f:@ line :function) 'f:map)
     (f:seq "idk yet"))
    ((string= (f:@ line :function) "")
     (f:seq "()"))
    ((string= (f:@ line :function) "with")
     (f:seq "with"
            (compile-line (f:first (f:@ line :args)))
            ";"))
    ((string= (f:@ line :function) "use")
     (f:seq "use"
            (compile-line (f:first (f:@ line :args)))
            ";"))
    ((string= (f:@ line :function) "list")
     (list-compile (compile-to-ada (f:@ line :args))))
    ((string= (f:@ line :function) "arglist")
     (arglist-compile (compile-to-ada (f:@ line :args))))
    ((string= (f:@ line :function) "defun")
     (f:seq "function"
            (compile-line (f:first (f:@ line :args)))
            (compile-line (f:@ (f:@ line :args) 2))
            "return"
            (compile-line (f:@ (f:@ line :args) 1))
            "is"
            "begin"
            "return"
            (compile-line (f:@ (f:@ line :args) 3))
            ";"
            "end"
            ";"))
    ((string= (f:@ line :function) "+")
     (addition-compile (compile-to-ada (f:@ line :args))))
    ((string= (f:@ line :function) "dot")
     (dot-compile (compile-to-ada (f:@ line :args))))
    ((string= (f:@ line :function) "main")
     (f:seq "begin"
            (compile-to-ada (f:@ line :args))
            "end"))
    (t
     (f:seq (f:@ line :function)
            "("
            (compile-to-ada (f:@ line :args))
            ")"))))


(defun compile-to-ada (ast)
  (f:image #'compile-line ast))

(defun compile-ada-file (ast)
  (if (or (not (string= (f:@ (f:first ast) :op) "func"))
          (not (string= (f:@ (f:first ast) :function) "ns")))
      (error "requires a namespace")
      (let ((compiled-file
              (f:concat
               (f:seq "Procedure")
               (f:seq (compile-line (f:first (f:@ (f:first ast) :args))))
               (f:seq "is")
               (compile-to-ada (f:less-first ast)))))
        
        compiled-file)))

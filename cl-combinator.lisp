;;;; cl-combinator.lisp

(in-package #:cl-combinator)

(defun make-state (remaining &optional (labels (f:empty-seq)) (position 0) (line 1))
  (f:map
   (:state 'SUCCESS)
   (:errors (f:empty-seq))
   (:result (f:empty-seq))
   (:remaining remaining)
   (:position position)
   (:line line)
   (:labels labels)))


(defun read-in (str)
  (make-state (f:convert 'f:seq (coerce str 'list))))


(defmacro with-state (binding &rest body)
  "Curry + check for errors on your state automatically"
  `(lambda (,binding)
     (if (equalp (f:@ ,binding :state) 'FAILURE)
         ,binding
         ,@body)))


(defun add-label (state label)
  (f:map-union state (f:map (:labels (f:with-last (f:@ state :labels)
                                       (concatenate 'string label " LINE: " (write-to-string (f:@ state :line)) " POSITION: " (write-to-string (f:@ state :position))))))))


(defmacro with-state-and-label (binding label &rest body)
  "Curry + check for errors on your state automatically + add parser label"
  `(lambda (,binding)
     (if (equalp (f:@ ,binding :state) 'FAILURE)
         ,binding
         (let ((,binding (add-label ,binding ,label)))
           ,@body))))


;;
;; Parsers
;;


(defun char (ch)
  (with-state state
    (if (f:first (f:@ state :remaining))
        (condp char= (f:first (f:@ state :remaining))
          (ch
           (if (char= ch #\linefeed)
               (f:map-union state (f:map
                                   (:state 'SUCCESS)
                                   (:remaining (f:less-first (f:@ state :remaining)))
                                   (:position 0)
                                   (:line (+ 1 (f:@ state :line)))
                                   (:result (f:with-last (f:@ state :result) ch))))
               (f:map-union state (f:map
                                   (:state 'SUCCESS)
                                   (:remaining (f:less-first (f:@ state :remaining)))
                                   (:position (+ 1 (f:@ state :position)))
                                   (:result (f:with-last (f:@ state :result) ch))))
               ))
          (if (char= (f:first (f:@ state :remaining)) #\linefeed)
              (f:map-union state (f:map
                                  (:state 'FAILURE)
                                  (:position 0)
                                  (:line (+ 1 (f:@ state :line)))
                                  (:errors (f:with-last (f:@ state :errors) (format nil "Expected character ~c, instead found ~c" ch (f:first (f:@ state :remaining)))))))
              (f:map-union state (f:map
                                  (:state 'FAILURE)
                                  (:position (+ 1 (f:@ state :position)))
                                  (:errors (f:with-last (f:@ state :errors) (format nil "Expected character ~c, instead found ~c" ch (f:first (f:@ state :remaining)))))))))
        (f:map-union state (f:map
                            (:state 'FAILURE)
                            (:position (+ 1 (f:@ state :position)))
                            (:errors (f:with-last (f:@ state :errors) (format nil "End of input"))))))))


(defun many (parsers)
  (with-state state
    (f:reduce (lambda (acc parser)
                (funcall parser acc))
              parsers
              :initial-value state)))


(defun choice (parsers)
  (with-state state
    (if (seq-to-list parsers)
        (let ((new-state (funcall (f:first parsers) state)))
          (if (equalp (f:@ new-state :state) 'FAILURE)
              (if (seq-to-list (f:less-first parsers))
                  (funcall (choice (f:less-first parsers)) state)
                  new-state)
              new-state))
        state)))


(defun block (parser)
  (with-state state
    (let ((n-block (funcall parser (make-state (f:@ state :remaining) (f:@ state :labels) (f:@ state :position) (f:@ state :line)))))
      (f:map-union state (f:map
                          (:state (f:@ n-block :state))
                          (:labels (f:@ n-block :labels))
                          (:position (f:@ n-block :position))
                          (:line (f:@ n-block :line))
                          (:result (f:with-last (f:@ state :result) (f:@ n-block :result)))
                          (:remaining (f:@ n-block :remaining))
                          (:errors (f:@ n-block :errors)))))))


(defun null (parser)
  (with-state state
    (let ((new-state (funcall parser (make-state (f:@ state :remaining) (f:@ state :labels) (f:@ state :position) (f:@ state :line)))))
      (if (seq-to-list (f:@ new-state :errors))
          new-state
          (f:map-union state (f:map
                              (:state (f:@ new-state :state))
                              (:position (f:@ new-state :position))
                              (:line (f:@ new-state :line))
                              (:result (f:@ state :result))
                              (:remaining (f:@ new-state :remaining))
                              (:errors (f:@ new-state :errors))))))))


(defun ? (parser)
  (with-state state
    (let ((new-state (funcall parser state)))
      (if (equalp (f:@ new-state :state) 'FAILURE)
          state
          new-state))))


(defun 0+ (parser)
  (with-state state
    (let ((new-state (funcall parser state)))
      (if (equalp (f:@ new-state :state) 'FAILURE)
          state
          (funcall (0+ parser) new-state)))))


(defun 1+ (parser)
  (with-state state
    (let ((new-state (funcall parser state)))
      (if (equalp (f:@ new-state :state) 'FAILURE)
          new-state
          (funcall (0+ parser) new-state)))))


(defun until-consumed (parser)
  (with-state state
    (let ((new-state (funcall parser state)))
      (if (equalp (f:@ new-state :state) 'FAILURE)
          (if (seq-to-list (f:@ new-state :remaining))
              new-state
              state)
          (funcall (until-consumed parser) new-state)))))


(defun letter ()
  (with-state state
    (funcall (choice (f:seq (char #\a) (char #\b) (char #\c) (char #\d) (char #\e)
                            (char #\f) (char #\g) (char #\h) (char #\i) (char #\j)
                            (char #\l) (char #\m) (char #\n) (char #\o) (char #\p)
                            (char #\q) (char #\r) (char #\s) (char #\t) (char #\u)
                            (char #\v) (char #\x) (char #\y) (char #\z)))
             state)))


(defun digit ()
  (with-state state
    (funcall (choice (f:seq (char #\0) (char #\1) (char #\2) (char #\3) (char #\4)
                            (char #\5) (char #\6) (char #\7) (char #\8) (char #\9))) state)))


(defun ws ()
  (with-state state
    (funcall (null (choice (f:seq (char #\ )
                              (char #\linefeed )
                              (char #\tab )))) state)))


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


(defun value ()
  "A value is a variable, a number, etc"
  (with-state-and-label state "value"
    (funcall (choice (f:seq (float) (word) (number)))
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
      ((seq-to-list (f:intersection (f:set #\.)
                                    arg-as-set))
       (f:map (:type 'FLOAT)
              (:value (seq-to-string arg))))
      ((seq-to-list (f:intersection (f:set #\0 #\1 #\2 #\3 #\4 #\5 #\6
                                           #\7 #\8 #\9)
                                    arg-as-set))
       (f:map (:type 'INT)
              (:value (seq-to-string arg))))
      ((seq-to-list (f:intersection (f:set #\a #\b #\c #\d #\e #\f #\g #\h #\i
                                           #\j #\k #\l #\m #\n #\o #\p #\q #\r
                                           #\s #\t #\u #\v #\w #\x #\y #\z)
                                    arg-as-set))
       (f:map (:type 'VAR)
              (:value (seq-to-string arg))))
      (t
       (f:map (:type 'N/A)
              (:value (seq-to-string arg)))))))


(defun build-sexpr (expression)
  (f:map (:function (build-func (f:first expression)))
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

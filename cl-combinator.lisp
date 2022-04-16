;;;; cl-combinator.lisp

(in-package #:cl-combinator)

(defun make-state (remaining &optional (labels (f:empty-seq)))
  (fset:map
   (:state 'SUCCESS)
   (:errors (f:empty-seq))
   (:result (f:empty-seq))
   (:remaining remaining)
   (:position 0)
   (:line 0)
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
  (f:map-union state (f:map (:labels (f:with-last (f:@ state :labels) label)))))

(defmacro with-state-and-label (binding label &rest body)
  "Curry + check for errors on your state automatically + add parser label"
  `(lambda (,binding)
     (if (equalp (f:@ ,binding :state) 'FAILURE)
         ,binding
         (let ((,binding (add-label ,binding ,label)))
           ,@body))))


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
          (f:map-union state (f:map
                              (:state 'FAILURE)
                              (:position (+ 1 (f:@ state :position)))
                              (:errors (f:with-last (f:@ state :errors) (format nil "Expected character ~c, instead found ~c~%position: ~s" ch (f:first (f:@ state :remaining))
                                                                                (f:@ state :position)))))))
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
    (let ((new-state (funcall (f:first parsers) state)))
      (if (equalp (f:@ new-state :state) 'FAILURE)
          (if (f:less-first parsers)
              (funcall (choice (f:less-first parsers)) state)
              new-state)
          new-state))))


(defun block (parser)
  (with-state state
    (let ((n-block (funcall parser (make-state (f:@ state :remaining) (f:@ state :labels)))))
      (f:map-union state (f:map
                          (:state (f:@ n-block :state))
                          (:labels (f:@ n-block :labels))
                          (:position (+ (f:@ state :position)
                                        (f:@ n-block :position)))
                          (:result (f:with-last (f:@ state :result) (f:@ n-block :result)))
                          (:remaining (f:@ n-block :remaining))
                          (:errors (f:@ n-block :errors)))))))


(defun null (parser)
  (with-state state
    (let ((new-state (funcall parser (make-state (f:@ state :remaining) (f:@ state :labels)))))
      (if (f:convert 'list (f:@ new-state :errors))
          new-state
          (f:map-union state (f:map
                              (:state (f:@ new-state :state))
                              (:position (+ (f:@ state :position)
                                            (f:@ new-state :position)))
                              (:result (f:@ state :result))
                              (:remaining (f:@ new-state :remaining))
                              (:errors (f:@ new-state :errors))))))))


(defun seq (parsers)
  (with-state state
    (funcall (many (f:image #'block parsers)) state)))


(defun ? (parser)
  (with-state state
    (let ((new-state (funcall parser state)))
      (if (equalp (f:@ new-state :state) 'FAILURE)
          state
          new-state))))


(defun 0+ (parser)
  (with-state-and-label state "0+"
    (let ((new-state (funcall parser state)))
      (if (equalp (f:@ new-state :state) 'FAILURE)
          state
          (funcall (zero-or-more parser) new-state)))))


(defun 1+ (parser)
  (with-state-and-label state "1+"
    (let ((new-state (funcall parser state)))
      (if (equalp (f:@ new-state :state) 'FAILURE)
          new-state
          (funcall (zero-or-more parser) new-state)))))


(defun letter ()
  (with-state-and-label state "letter"
    (funcall (choice (f:seq (char #\a) (char #\b) (char #\c) (char #\d) (char #\e)
                           (char #\f) (char #\g) (char #\h) (char #\i) (char #\j)
                           (char #\l) (char #\m) (char #\n) (char #\o) (char #\p)
                           (char #\q) (char #\r) (char #\s) (char #\t) (char #\u)
                           (char #\v) (char #\x) (char #\y) (char #\z))) state)))


(defun digit ()
  (with-state-and-label state "digit"
    (funcall (choice (f:seq (char #\0) (char #\1) (char #\2) (char #\3) (char #\4)
                           (char #\5) (char #\6) (char #\7) (char #\8) (char #\9))) state)))

(defun ws ()
  (with-state state
    (funcall (null (char #\ )) state)))

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


(defun except (chars)
  (with-state state
    (let ((acceptable-chars (f:filter (lambda (ch)
                                        (not (member ch (seq-to-list chars))))
                                      *all-chars*)))
      (if (seq-to-list acceptable-chars)
          (funcall (choice (f:image #'char acceptable-chars)) state)
          (funcall (char #\a) state)))))

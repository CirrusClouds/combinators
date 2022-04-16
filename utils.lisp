(in-package #:cl-combinator)

(defmacro condp (test predicate &body forms)
  `(cond
     ,@(mapcar (lambda (cc)
                 `((,test ,(first cc) ,predicate) (progn
                                                    ,@(rest cc))))
               (butlast forms))
     (t
      ,@(last forms))))

(defun empty? (x)
  (not (fset:convert 'list x)))

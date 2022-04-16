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

(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun seq-to-string (ch-seq)
  (f:convert 'string ch-seq))

(defun seq-to-list (s)
  (f:convert 'list s))

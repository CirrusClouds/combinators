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


(setq *alpha-chars* (f:seq #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o
                           #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

(setq *digit-chars* (f:seq #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(setq *special-chars* (f:seq #\- #\* #\+ #\/ #\" #\; #\: #\' #\_ #\< #\? #\\ #\( #\)
                             #\! #\£ #\$ #\% #\~ #\# #\  #\linefeed #\tab #\& #\^ ))

(setq *all-chars* (f:concat *alpha-chars* *digit-chars* *special-chars*))

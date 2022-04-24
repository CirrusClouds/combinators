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


(defvar *alpha-chars* (f:seq #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o
                           #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\A #\B #\C #\D
                           #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S
                           #\T #\U #\V #\W #\X #\Y #\Z))

(defvar *digit-chars* (f:seq #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defvar *special-chars* (f:seq #\- #\* #\+ #\/ #\" #\; #\: #\' #\_ #\< #\? #\\ #\( #\)
                             #\! #\£ #\$ #\% #\~ #\# #\  #\linefeed #\tab #\& #\^ #\.))

(defvar *all-chars* (f:concat *alpha-chars* *digit-chars* *special-chars*))


(defun i-reduce (f acc coll &optional (i 0))
  (if coll
      (i-reduce f (funcall f i acc coll) (rest coll) (+ i 1))
      acc))

(defun i-filter (f coll &optional (i 0))
  (if coll
      (if (funcall f i (first coll))
          (cons (first coll) (i-filter f (rest coll) (+ i 1))))
      nil))

(defmacro ->> (&rest r)
  "Tail-led pipelining/threading"
  (reduce (lambda (o i)
            `(,@i ,o)) r))

(defmacro -> (&rest r)
  "Elixir-style front-running pipelining/threading"
  (reduce (lambda (o i)
            `(,(car i) ,o ,@(cdr i))) r))


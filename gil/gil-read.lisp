;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage gil-read
  (:use :common-lisp :generic :denest)
  (:documentation "Stream reader for GIL.
TODO needs to filter out whitespace gil-execute")
  (:export gil-read execute))

(in-package :gil-read)

(defvar *buffer-len* 1024)
(defvar *buffer* (make-string *buffer-len*))

(defun gil-read-col (stream &key funlike)
  (collecting () (gil-read stream :fn #'collecting :funlike funlike)))

(defun gil-read (stream &key fn (buf-i 0) (digit 0) funlike)
  (labels ((add-buffer (ch)
	     (setf (aref *buffer* buf-i) ch
		   buf-i (+ buf-i 1))
	     (when (= buf-i *buffer-len*)
	       (dump-buffer)))
	   (dump-buffer ()
	     (when (> buf-i 0)
	       (if (> digit 0)
		 (let*((f (- buf-i digit))
		       (n (read-from-string
			   (subseq *buffer* f buf-i))))
		   (unless (= f 0)
		     (funcall fn (subseq *buffer* 0 f)))
		   (funcall fn n))
		 (funcall fn (subseq *buffer* 0 buf-i)))
	       (setf buf-i 0))
	     (setf digit 0)
	     (values)))
    (do ((ch (read-char stream nil nil) (read-char stream nil nil)))
	((not ch) (dump-buffer))
      (case ch
	(#\(
	 (dump-buffer)
	 (let ((read (read stream)))
	   (funcall fn `(,read ,@(gil-read-col stream :funlike t))))
	 (setf funlike nil)) ;Otherwise we'll miss whitespace.
	(#\)
	 (dump-buffer)
	 (return))
	(#\\ ;Escape character.
	 (setq funlike nil)
	 (when-let ch (read-char stream nil nil)
	   (add-buffer ch)))
	(#\$ ;Force (nearly)regular lisp.
	 (dump-buffer)
	 (funcall fn (read stream))
	 (unread-char #\Space stream))
	(#\#
	 (cond
	   ((char= #\( (read-char stream))
	    (dump-buffer)
	    (funcall fn `(gil:glist :series ,@(gil-read-col stream))))
	   (t
	    (add-buffer #\#))))
	((#\Space #\Newline #\Tab)
	 (when (or (> digit 0) (> buf-i (/ *buffer-len* 2)))
	   (dump-buffer)) ;If integer, dump integer, if word finished and 
	                  ;more than halfway buffer, dump buffer.
	 (setf digit 0)
	 (unless funlike
	   (add-buffer ch)))
	(t
	 (cond
	   ((< digit 0)
	    (setf funlike nil))
	   ((or (digit-char-p ch) (and (char= ch #\.) (> digit 0)))
	    (setf- + digit 1))
	   (t
	    (setf funlike nil
	          digit -1)))
	 (add-buffer ch))))))

(defpackage :gil-user
  (:use :cl :gil :gils :gil-style :denest))

(defun execute-file (file-name)
  (gil:glist-list :series
    (denest
     (collecting (nil list))
     (flet ((collect-eval (item)
	      (collecting (eval item)))))
     (let ((len (length file-name)))
       (with-open-file (stream file-name)
	 (if (string= (subseq file-name (- len 5)) ".lisp")
	   (do ((read (read stream nil :done) (read stream nil :done)))
	       ((eql read :done) nil)
	     (collect-eval read))
	   (gil-read stream :fn #'collect-eval)))))))

(defun execute (from)
  "Reads and then produces using whatever is in gil:*lang*. 
You need to funcall output if you're not using it in-line;
see gil-execute if you want to output.

TODO track checksums(keyword currently ignored), and relevant arguments.
 (If arguments change it will need to be redone regardless.)"
  (cond
    ((functionp from)
     from)
    ((listp from)
     (lambda ()
       (mapcar (lambda (el)
		 (funcall (execute el))) from)))
    ((stringp from)
     (execute-file from))
    (t
     (error "~a not recognized" from))))

 #|(defun gil-execute
    (from-file &key (to-file "doc.html") (to-path ".")
     (use-package '(:cl :gil :gils :gil-read))
     (funcall t))
  "Execute, but funcalls for you, and arguments set the special variables\
 accordingly."
  (let*((*default-pathname-defaults*
	 (pathname(format nil "~D~D"
		    *default-pathname-defaults* to-path)))
	(*standard-output*
	 (open to-file :direction :output
	       :if-does-not-exist :create :if-exists :supersede))
	(pkg-name (format nil "gil-file-~a" from-file)))
    (eval `(defpackage ,pkg-name (:use ,@use-package)))
    (execute from-file :fn
	     (lambda (item)
	       (let ((evalled (eval `(progn (in-package ,pkg-name)
					    ,item))))
		 (if funcall (funcall evalled) evalled))))
    (close *standard-output*)))|#

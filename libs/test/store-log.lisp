
;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :store-log-test
  (:use :common-lisp :generic :cl-fad :store-log))

(in-package :store-log-test)

;;TODO randomly do stuff like adding blog entries.

(defvar *written*)
(defvar *write-nr*)
(defvar *check-nr*)

(defvar *test-log*)

(defclass rnd-log (base-log)
  ((rnd :initarg :rnd :type integer :initform (random 10000) :reader rnd)))
(defclass rnd-entry (base-entry)
  ((rnd :initarg :rnd :type integer :reader rnd)))

(defmethod add-directory :around
    ((log rnd-log) (file pathname) &key (time (get-universal-time)))
  (change-class (call-next-method :time time) 'rnd-entry
		:rnd (with-open-file (stream file)
		       (read stream))))

(defun write-file
    (k rnd rnd-inside &optional
     (filename (format nil "~a-~a-~a" rnd k (setf- + *write-nr* 1))))
  "Write file and denote that it is there."
  (with-open-file (stream filename :direction :output
		   :if-exists :error :if-does-not-exist :create)
    (format stream "~a" rnd-inside))
  (push (make-instance 'rnd-entry :file filename :rnd rnd-inside)
	*written*))
      
(defun random-write (cnt)
  "Write a bunch of files, denote they are there."
  (dotimes (k cnt)
    (write-file k (random 1000) (random 1000))))

(defun check-correspondence ()
  (setf- + *check-nr* 1)
  (flet ((sortfn (a b)
	   (string> (namestring(file a)) (namestring(file b)))))
    (mapcar (lambda (writ entry)
	      (with-mod-slots w- (file had-timestamp rnd) writ
		(with-mod-slots e- (file had-timestamp rnd) entry
		  (assert (string= (namestring w-file) (namestring e-file))
			  nil "Files ~a ~a" w-file e-file)
		  (assert (= e-had-timestamp (file-write-date e-file))
			  nil "Times")
		  (assert (= w-rnd e-rnd) nil
			  "Not same data; ~a ~a" w-rnd e-rnd))))
	    (setf- sort *written* #'sortfn)
	    (setf- sort (slot-value *test-log* 'entries) #'sortfn)))
  (assert (= (length *written*) (length (entries *test-log*)))
	  nil "Element count. denoted ~a(~a), has ~a~2%~a~2%~a"
	  (length *written*) *write-nr* (length (entries *test-log*))
	  *written* (entries *test-log*)))

(defparameter *testing-directory* 
  #p"/home/jasper/proj/lisp-editor/libs/test/log-plaything/")

(defmethod add-entry :around ((log rnd-log) add
			      &key (time (get-universal-time)))
  (print :nexted)
  (let ((entry (call-next-method log add :time time)))
    (change-class entry 'rnd-entry
		  :rnd (with-open-file (stream (file entry))
			 (read stream)))))

(defun test (steps files-per-step)
  "Steps a bunch of steps, doing random stuff."
  (when (file-exists-p *testing-directory*) ;Remove previous play.
    (delete-directory-and-files *testing-directory*))
  (ensure-directories-exist *testing-directory*)
 ;Make directory to test in.
  (let ((*default-pathname-defaults* *testing-directory*)
	(*written* (list (make-instance 'rnd-entry 
			   :file "test-log" :rnd 0)))
	(*write-nr* 0) (*check-nr* 0)
	(data (random 1.0)))
    ;Initialize, save.
    (with-log (*test-log* "test-log")
      (when (= 0 (random 2))
	(random-write (random files-per-step))))
    
    (dotimes (step steps)
      (with-log (*test-log* "test-log")
	(random-write (random files-per-step))
	(add-entry *test-log* :new-files)
	(check-correspondence)))
    
    (with-log (*test-log* "test-log")
      (assert (= (rnd *test-log*) data) nil "Data did not match.")
      (check-correspondence)))
  (delete-directory-and-files *testing-directory*))

(test 20 20)

(make-instance 'base-log :from-log "lala")

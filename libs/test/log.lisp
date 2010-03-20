
;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :log-test
  (:use :common-lisp :generic :cl-fad :log))

(in-package :log-test)

;;TODO randomly do stuff like adding blog entries.

(defvar *written*)
(defvar *write-nr*)
(defvar *check-nr*)

(defun write-file
    (k rnd rnd-inside &optional
     (filename (format nil "~a-~a-~a" rnd k (gen:setf- + *write-nr* 1))))
  "Write file and denote that it is there."
  (with-open-file (stream filename :direction :output
			  :if-exists :error :if-does-not-exist :create)
    (format stream "~a" rnd-inside))
  (push (list (file-namestring filename) rnd-inside (get-universal-time))
	*written*))
      
(defun random-write (cnt)
  "Write a bunch of files, denote they are there."
  (dotimes (k cnt)
    (write-file k (random 1000) (random 1000))))

(defun check-correspondence ()
  (gen:setf- + *check-nr* 1)
  (flet ((sortfn (a b)
	   (string> (car a) (car b))))
    (mapcar (lambda (writ entry)
	      (destructuring-bind (file rnd-inside writ-time) writ
		(declare (ignore rnd-inside writ-time))
		(with-entry-access entry
		  (assert (string= filename file) nil
			  "Files ~a ~a" writ entry)
		  (assert (= had-timestamp (file-write-date file))
			  nil "Times"))))
	    (gen:setf- sort *written* #'sortfn)
	    (gen:setf- sort log:*log-entries* #'sortfn)))
  (assert (= (length *written*) (length log:*log-entries*))
	  nil "Element count. denoted ~a(~a), has ~a~2%~a~2%~a"
	  (length *written*) *write-nr* (length log:*log-entries*)
	  *written* log:*log-entries*))

(defparameter *testing-directory* 
  #p"/home/jasper/proj/lisp-editor/tools/test/log-plaything/")

(defun test (steps files-per-step)
  "Steps a bunch of steps, doing random stuff.
Only tests basic stuff, nothing beyond-with gil-log::update-entry
WARNING lots of writes, dont do with NAND storage, or storages that don't\
 handle frequent writes!
NOTE if you run multiple in the same directory at the same time, nothing\
 else than a error is to be expected."
  (when (file-exists-p *testing-directory*) ;Remove previous play.
    (delete-directory-and-files *testing-directory*))
  (ensure-directories-exist *testing-directory*)
 ;Make directory to test in.
  (let ((*default-pathname-defaults* *testing-directory*)
	(*written* nil) (*write-nr* 0) (*check-nr* 0)
	(data (random 1.0)))
    (with-log ()
      (setf (log-data :data) data))
    (dotimes (step steps)
      (with-log ()
	(random-write (random files-per-step))
	(add-new-entries ".")
	(check-correspondence)))
    (with-log ()
      (assert (= (log-data :data) data) nil "Data did not match.")
      (check-correspondence)))
  (delete-directory-and-files *testing-directory*))

(test 20 20)


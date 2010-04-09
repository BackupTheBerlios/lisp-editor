
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

(defun check-correspondence (&key (n 0))
  (setf- + *check-nr* 1)
  (dolist (writ *written*)
    (with-mod-slots w- (file had-timestamp rnd) writ
      (let ((entry (get-entry *test-log* w-file)))
	(assert entry nil "Entry not found for ~a" w-file)
	(with-mod-slots e- (file had-timestamp rnd) entry
	  (assert (eql (type-of writ) (type-of entry)) nil
		  "Incorrect typing. ~a!=~a, ~s"
		  (type-of writ) (type-of entry) w-file)
	  (when (eql (type-of writ) 'rnd-entry)
	    (assert (= w-rnd e-rnd) nil
		    "Not same data; ~a ~a" w-rnd e-rnd))))
      (setf- + n 1)))
  (assert (= (length *written*) (entry-cnt *test-log*) n)
    nil "Element count. denoted ~a(~a), has ~a, found ~a"
    (length *written*) *write-nr* (entry-cnt *test-log*) n))

(defparameter *testing-directory* 
  #p"/home/jasper/proj/lisp-editor/libs/test/log-plaything/")

(add-entry-hook ((log rnd-log) entry)
  (let ((read (with-open-file (stream (file entry))
		 (read stream))))
    (if (numberp read)
      (change-class entry 'rnd-entry :rnd read)
      entry)))

(defun test (steps files-per-step)
  "Steps a bunch of steps, doing random stuff and checking consistency."
  (when (file-exists-p *testing-directory*) ;Remove previous play.
    (delete-directory-and-files *testing-directory*))
  (ensure-directories-exist *testing-directory*)
 ;Make directory to test in.
  (let ((*default-pathname-defaults* *testing-directory*)
	(*written* (list (make-instance 'base-entry 
			   :file "test-log")))
	(*write-nr* 0) (*check-nr* 0)
	(data (random 1.0)))
   ;Initialize, save. 
    (with-log (*test-log* (make-instance 'rnd-log
			    :file "test-log" :rnd data))
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

(time (test 20 4))

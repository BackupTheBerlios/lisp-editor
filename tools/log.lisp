;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :log
  (:use :common-lisp :generic :cl-fad)
  (:export init with-init *entries*
	   add-entry add-directory-entries remove-entry
	   find-entry-name find-entry-file
	   name file keywords links entry-time)
  (:documentation "Very basic logger.

The reason to put data in a single file is because adding it to the 
separate files might cause it to be more spread out.

You look at the contents directly via *entries*
slot values links may have qualifiers (link-type ...files..)

Note: this might be a noobish way of doing it. The filename system is\
 database too, perhaps the extra data should be attached to the files\
 themselves."))

(in-package :log)

(defvar *last-write*)

(defvar *log-name*)
(defvar *directory*)
(defvar *entries*)

(defun write-data-line (object stream)
  (format stream "~{~s~^ ~}~%" object))

(defun read-data-line (stream)
  (read-from-string (format nil "(~a)" (read-line stream))))

(defclass log-entry ()
  ((entry-name :initarg :entry-name :type string :reader name)
   (file-name :initarg :file-name :type string :reader file)
   (keywords :initarg :keywords :type list :accessor keywords)
   (links :initarg :links :type list :accessor links
     :documentation "Links to other entries, by file-name.")
   (entry-time :initarg :entry-time :type integer :reader entry-time)
   (data :initarg :data :type list :reader data)))

(defun make-entry (file-name &key (entry-name file-name) keywords links
		                   (entry-time (get-universal-time)))
  (make-instance 'log-entry
    :entry-name entry-name :file-name file-name :entry-time entry-time
    :keywords keywords :links links))

(defun read-entry (stream)
  (destructuring-bind (entry-name file-name entry-time keywords links
		       &rest data) 
      (read-data-line stream)
    (make-entry file-name :entry-name entry-name :entry-time entry-time
		:keywords keywords :links links :data data)))

(defun write-entry (entry stream)
  (with-slots (entry-name file-name entry-time keywords links) entry
    (write-data-line (list entry-name file-name entry-time keywords links)
		     stream)))

(defun find-entry-name (name)
  (find-if (lambda (el) (string= name (name el))) *entries*))
(defun find-entry-file (file)
  (find-if (lambda (el) (string= file (file el))) *entries*))

(defun log-file ()
  (format nil "~a~a" *directory* *log-name*))

(defun read-log ()
  "Reads log."
  (with-open-file (log-stream (log-file))
    (let ((first-read (read-data-line log-stream)))
      (assert (not(eql first-read :superseded)) nil
	      "Did you superseed the by some other log system?")
      (destructuring-bind (directory entry-cnt last-write) first-read
	(setq *directory* directory
	      *entries* nil
	      *last-write* last-write)
	(dotimes (k entry-cnt)
	  (push (read-entry log-stream) *entries*))))
    (let ((at-end (read log-stream)))
      (assert (case at-end ((:e :end) t)) nil
	"Error expect :end or :e at end of log file. got ~s" at-end))))

(defun write-log (&key (if-does-not-exist :error))
  "Writes current log data."
  (with-open-file (log-stream (log-file) :direction :output
		   :if-does-not-exist if-does-not-exist
		   :if-exists :supersede)
    (write-data-line (list *directory* (length *entries*) 
			   (get-universal-time))
	   log-stream)
    (dolist (entry *entries*)
      (write-entry entry log-stream))
    (format log-stream ":e")))

(defun init (&key (if-does-not-exist :error))
  "If asked creates a log, then reads."
  (unless (probe-file (log-file))
    (case if-does-not-exist
      (:create
       (let ((*entries* nil))
	 (ensure-directories-exist *directory*)
	 (write-log :if-does-not-exist :create)))
      (:error 
       (error "Log file ~a does not exist." (log-file)))
      (t
       (return-from init if-does-not-exist))))
  (read-log))

(defmacro with-init ((&key (log-name ".log") (directory "log/")
			   (if-does-not-exist :error))
		     &body body)
  "Makes the variables local inside the body, and write-log at end."
  `(let ((*log-name* ,log-name) (*directory* ,directory)
	 (*entries* nil) (*last-write* 0))
     (init :if-does-not-exist ,if-does-not-exist)
     ,@body
     (write-log)))

(defun entry-registered (file-name)
  (position-if (lambda (entry) (string= file-name (file entry)))
	       *entries*)) ;At some point, you'll need a hashtable.

(defun add-entry (file-name &key (entry-name file-name) keywords links
		                 (entry-time (get-universal-time))
		                 (if-exists :error))
  "Adds an entry."
  (when-let i (entry-registered file-name)
    (case if-exists
      (:error 
       (error "File under name ~a already exists." file-name))
      (:rename
       (do ((k 0 (+ k 1)))
	   ((not (entry-registered (format nil "~a[~a]" file-name k)))
	    (return-from add-entry 
	      (add-entry (format nil "~a[~a]" file-name k))))
	 (when (> k 1000000000)
	   (error "Not designed for these kinds of numbers,\
 obviously."))))
      (:supersede
       (setq *entries* (append (subseq *entries* 0 (- i 1))
			       (subseq *entries* i))))
      (:append
       (let ((entry (nth i *entries*)))
	 (assert (string= entry-name (name entry)) nil
	   "If you want to append to an entry, their names must match.")
	 (with-mod-slots e- (keywords links) entry
	   (setf- append e-keywords keywords)
	   (setf- append e-links links))
	 (return-from add-entry entry)))))
  (car (push (make-entry file-name
	       :entry-name entry-name :entry-time entry-time
	       :keywords keywords :links links)
	     *entries*)))

(labels ((recursive-list (dirname)
	   (mapcan (lambda (el)
		     (if (directory-pathname-p el)
		       (recursive-list el)
		       (list el)))
		   (list-directory dirname))))
  (defun add-directory-entries
      (&key (dirname *default-pathname-defaults*) 
       (no-path t) recursive (hook #'identity))
    "Add entries to the log that aren't there yet."
    (let ((list (if recursive (recursive-list dirname)
		  (let ((list (list-directory dirname)))
		    (if no-path
		      (remove-if #'cl-fad:directory-pathname-p list)
		      list)))))
      (dolist (el list)
	(unless (find-if (lambda (other)
			   (or (string= (name el) (name other))
			       (string= (file el) (file other))))
			 *entries*)
	  (push (funcall hook (make-entry el :file-name el
					  :keywords '(:directory-added)))
		*entries*))))))
      
(defun remove-entry
    (file-name &key delete-dont-ask (delete delete-dont-ask)
     (if-does-not-exist :error))
  "Remove an entry by file name."
  (setq *entries*  ;TODO also remove the file.
	(remove-if (lambda (entry) (string= (file entry) file-name))
		   *entries*))
  (cond ((not delete))
	(delete-dont-ask
	 (delete-directory-and-files file-name))
	(t
	 (format t "Are you sure you want to delete ~a recursively.
y/n~%"
		 file-name)
	 (do
	  ((ch (read-char) (read-char)))
	  ((case ch ((#\n #\y #\N #\Y) t))
	   (case ch ((#\y #\Y)
		     (delete-directory-and-files file-name
		       :if-does-not-exist if-does-not-exist))))))))

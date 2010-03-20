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
  (:use :common-lisp :alexandria)
  (:export *log-file* log-data *log-entries*
           read-log write-log with-log
	   with-entry-access access filename seen-date had-timestamp
	   add-entry add-new-entries
	   entry-changed-p)
  (:documentation "Logs files, when they change, allows to store.
Write-log writes the all of *log-file*"))

(in-package :log)

(defvar *log-file* ".log")
(defvar *log-data* nil)
(defvar *log-entries* nil)

(defun log-data (name)
  (getf *log-data* name))
(defun (setf log-data) (to name)
  (setf (getf *log-data* name) to))

(defun read-log (&optional (log-file *log-file*))
  "Reads/initias log."
  (unless (cl-fad:file-exists-p log-file)
    (setq *log-data* nil *log-entries* nil)
    (return-from read-log (values)))
  (with-open-file (stream log-file :direction :input)
    (setq *log-file* (read stream)
	  *log-data* (read stream))
    (do ((read (read stream nil nil) (read stream nil nil)))
	((null read) (values))
      (push read *log-entries*)))
  (setf (log-data :last-read) (get-universal-time)))

(defun write-log (&optional (log-file *log-file*))
  "Writes current log state."
  (setf (log-data :last-write) (get-universal-time))
  (with-open-file (stream log-file :direction :output
		   :if-exists :supersede :if-does-not-exist :create)
    (print *log-file* stream)
    (print *log-data* stream)
    (mapcar (rcurry #'print stream) *log-entries*))
  (values))

(defmacro with-log ((&key log-file)
		    &body body)
  "Read, do some stuff, write again, with all variables isolated within."
  `(let (*log-entries* *log-data*)
     (read-log ,@(when log-file (list log-file)))
     (prog1 (progn ,@body)
       (write-log ,@(when log-file (list log-file))))))

(defun add-entry (file-namestring &key (time (get-universal-time)))
  "Reads new entries in a directory. (Make sure read-log been called)"
  (unless (or (string= file-namestring *log-file*)
	      (assoc file-namestring *log-entries* :test #'string=))
    (setf (log-data :last-addition) time)
    (push (list file-namestring time (file-write-date file-namestring))
	  *log-entries*)))

(defun add-new-entries
    (dirname &key recursive (max-depth 100) (time (get-universal-time)))
  "Reads new entries in a directory. (Make sure read-log been called)"
  (dolist (path (cl-fad:list-directory dirname))
    (if (cl-fad:directory-pathname-p path)
      (when (and recursive (> max-depth 0))
	(add-new-entries path :recursive t :max-depth (- max-depth 1)))
      (add-entry (file-namestring path) :time time))))

(defmacro with-entry-access (entry &body body)
  "Access an entry."
  `(destructuring-bind (filename seen-date had-timestamp
			&rest keywords) ,entry
     (declare (ignorable filename seen-date had-timestamp
			 keywords))
     (macrolet ((access (name)
		  `(getf keywords ,name)))
       ,@body)))

(defun entry-changed-p (entry)
  "Returns if the entry has changed. Looks by just checking timestamps."
  (with-entry-access entry
    (when (< had-timestamp (file-write-date filename))
      (setf (nth 3 entry) (file-write-date filename))
      t)))

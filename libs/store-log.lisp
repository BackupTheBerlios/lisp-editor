;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :store-log
  (:use :common-lisp :alexandria :cl-store)
  (:export
   base-log log-file entries last-read last-write last-addition last-action
   base-entry file last-encounter had-timestamp addition-time
   read-log add-entry with-log)
  (:documentation "Uses cl-store to keep track of files from a single\
 file, denoting when they're encountered/first added.

Use cl-store's store and restore read/write the log.
Extend by (defmethod store :around ((log newtype) (to (eql :default))).. 
and (defrestore-cl-store (newtype log) .."))

(in-package :store-log)

(defclass base-log ()
  ((log-file :initarg :log-file
	     :reader log-file
     :documentation "File the log came from.")
   (entries :initform nil :type list :reader entries
     :documentation "Entries of files tracked.")
   (last-read :initform 0 :type integer :reader last-read)
   (last-write :initform 0 :type integer :reader last-write)
   (last-addition :initform 0 :type integer :reader last-addition)
   (last-action :initform 0 :type integer :reader last-action))
  (:documentation "Log of files, the last-read/write etcetera are times\
 relating to the entire log."))

(defmethod store ((log base-log) (to (eql :default)) &optional designator)
  (store log (log-file log) designator))

(defrestore-cl-store (base-log place)
  (let ((log (restore-object place)))
    (with-slots (last-read) log
      (setf last-read (get-universal-time)))
    log))

(defclass base-entry ()
  ((file :type pathname :reader file)
   (last-encounter :initform 0 :initarg :last-encounter
		   :type integer :reader last-encounter)
   (had-timestamp :initform 0 :type integer 
		  :reader had-timestamp)
   (addition-time :initarg :addition-time
		  :initform 0 :type integer :reader addition-time)))

(defmethod initialize-instance :after 
    ((entry base-entry) &key file (time (get-universal-time)))
  (declare (type (or string pathname) file))
  (let ((file (typecase file
		(string   file)
		(pathname file))))
    (setf (slot-value entry 'file) file))
  (with-slots (last-encounter had-timestamp file) entry
    (setf last-encounter time
	  had-timestamp 
	  (or (when (probe-file file) (file-write-date file)) 0)))
  entry)

(defun file-in-log (log file)
  (find-if (lambda (entry)
	     (string= (format nil "~a" file) 
		      (format nil "~a" (file entry))))
	   (entries log)))

(defgeneric add-entry (into-log from &key time)
  (:documentation "Adds an entry to a log."))

(defmethod add-entry
    ((log base-log) (file string) &key (time (get-universal-time)))
  (declare (type integer time))
  (add-entry log (pathname file) :time time))

(defmethod add-entry
    ((log base-log) (file pathname) &key (time (get-universal-time)))
  (declare (type integer time))
  (with-slots (entries last-addition last-action) log
    (let ((found (file-in-log log file)))
      (cond
	(found
	 (setf (slot-value found 'last-encounter) time
	       last-action time)
	 found)
	(t
	 (setf last-addition time)
	 (car(push (make-instance 'base-entry :addition-time time
		     :file file :last-encounter time) entries)))))))

(defmethod add-entry
    ((log base-log) (entry base-entry) &key time)
  (declare (ignore time))
  (with-slots (entries) log
    (remove-if (lambda (e)
		 (string= (format nil "~a" (file e))
			  (format nil "~a" (file entry))))
	       entries)
    (car (push entry entries))))

(defmethod add-entry ((log base-log) (add (eql :new-files))
		      &key (time (get-universal-time)))
  (declare (type integer time))
  (dolist (path (cl-fad:list-directory *default-pathname-defaults*))
    (add-entry log (path-stuff:from-path-root (namestring path))
	       :time time)))

(defmethod add-entry ((log base-log) (add-list list) 
		      &key (time (get-universal-time)))
  (dolist (add add-list)
    (add-entry log add :time time)))

(defmethod add-entry ((log base-log) (add-log base-log)
		      &key (time (get-universal-time)))
  (add-entry log (entries add-log) :time time))

(defgeneric read-log (from)
  (:documentation "Reads a log."))

(defmethod read-log ((log base-log))
  (add-entry log (restore (file log))))

(defmethod read-log ((file string))
  (read-log (pathname file)))

(defmethod read-log ((file pathname))
  (if (probe-file file)
    (restore file)
    (make-instance 'base-log :log-file file)))

(defun with-log-fn (read-from fn &key (log (read-log read-from)))
  (prog1 (funcall fn log)
    (store log read-from)))

(defmacro with-log ((&optional (log-var (gensym)) read-from) &body body)
  "Reads a log, allows you to do body, then writes it again.
The if LOG argument 1 element, just a filename witht the log, or a log to\
 read from. Otherwise, log is taken to be arguments to make-instance."
  `(with-log-fn ,read-from (lambda (,log-var) ,@body)))

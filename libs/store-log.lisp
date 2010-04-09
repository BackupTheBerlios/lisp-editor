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
  (:use :common-lisp :alexandria :cl-store :path-stuff)
  (:export
   base-log *root* file get-entry map-entries-fn map-entries entry-cnt
   creation-time
   last-read last-write last-addition last-action
   base-entry last-encounter had-timestamp
   read-log add-entry add-entry-hook with-log)
  (:documentation "Uses cl-store to keep track of files from a single\
 file, denoting when they're encountered/first added.

Use cl-store's store to write and read-log to read the log. (both\
 extendable with methods.)
Use add-entry-hook to change the entry-type to something else than\
 base-entry.

With-log will read and later write for you."))

(in-package :store-log)

(defgeneric file (of))

(defmethod file ((string string))
  string)

(defmethod file ((pathname pathname))
  (namestring pathname))

(defvar *root* *default-pathname-defaults*
  "Root of the current log.")

(defclass log-event-times ()
  ((last-encounter :initform 0 :initarg :last-encounter
    :type integer :reader last-encounter)
   (creation-time :initform (get-universal-time) :initarg :creation-time
		  :type integer :reader creation-time)
   (had-timestamp :initform 0 :type integer :reader had-timestamp))
  (:documentation "Times of some of the events."))

(defclass base-log (log-event-times)
  ((root :initarg :root :initform (file *root*)
	 :type string :reader root
     :documentation "Root of the log, files are named relative to\
 the root. Set *default-pathname-defaults* accordingly.")
   (file :initarg :file :reader file :type string
     :documentation "File the log came from.")
   (entries :type hash-table 
	    :initform (make-hash-table :test 'equalp)
     :documentation "Entries of files tracked.")
   (last-read :initform 0 :type integer :reader last-read)
   (last-write :initform 0 :type integer :reader last-write)
   (last-addition :initform 0 :type integer :reader last-addition)
   (last-action :initform 0 :type integer :reader last-action))
  (:documentation "Log of files, the last-read/write etcetera are times\
 relating to the entire log."))

(defgeneric get-entry (log file))
(defgeneric (setf get-entry) (to log file))

(defmethod get-entry ((log base-log) (file string))
  (gethash file (slot-value log 'entries)))

(defmethod (setf get-entry) (to (log base-log) (file string))
  (setf (gethash file (slot-value log 'entries)) to))

(defmethod get-entry ((log base-log) (file pathname))
  (get-entry log (file file)))
(defmethod (setf get-entry) (to (log base-log) (file pathname))
  (setf (get-entry log (file file)) to))

(defgeneric map-entries-fn (log function))

(defmacro map-entries (log (entry &optional (file (gensym))) &body body)
  "Iterates over all the entries. (return) stops it"
  `(block nil
     (map-entries-fn ,log (lambda (,file ,entry)
			    (declare (ignorable ,file))
			    ,@body))))

(defmethod map-entries-fn ((log base-log) (function function))
  (declare (type (function (string t) (values)) function))
  (maphash function (slot-value log 'entries)))

(defgeneric entry-cnt (log))

(defmethod entry-cnt ((log base-log))
  (hash-table-count (slot-value log 'entries)))

(defmethod store ((log base-log) (to (eql :default)) &optional designator)
  (store log (file log) designator))

(defrestore-cl-store (base-log place)
  (let ((log (restore-object place)))
    (with-slots (last-read last-encounter had-timestamp) log
      (setf last-read (get-universal-time)
	    last-encounter last-read
	    had-timestamp (file-write-date place)))
    log))

(defclass base-entry (log-event-times)
  ((file :type pathname :reader file)
   (last-encounter :initform 0 :initarg :last-encounter
		   :type integer :reader last-encounter)))

(defmethod initialize-instance :after 
    ((entry base-entry) &key file (time (get-universal-time)))
  (declare (type (or string pathname) file))
  (let ((file (typecase file (string   file) (pathname file))))
    (setf (slot-value entry 'file) file))
  (with-slots (last-encounter had-timestamp file) entry
    (setf last-encounter time
	  had-timestamp (or (when (probe-file file) 
			      (file-write-date file))
			    0)))
  entry)

(defgeneric add-entry (into-log from &key time root)
  (:documentation "Adds an entry to a log."))

(defmethod add-entry
    ((log base-log) (file string)
     &key (time (get-universal-time)) (root *root*))
  (declare (type integer time) (ignore root))
  (with-slots (last-addition last-action last-encounter) log
    (let ((found (get-entry log file)))
      (cond
	(found
	 (setf (slot-value found 'last-encounter) time
	       last-action time)
	 found)
	(t
	 (setf last-addition time)
	 (add-entry log (make-instance 'base-entry :file file
				       :creation-time time)))))))

(defmethod add-entry
    ((log base-log) (file pathname) &key
     (time (get-universal-time)) (root *root*))
  (declare (type integer time))
  (add-entry log (file file) :time time :root root))

(defmethod add-entry
    ((log base-log) (entry base-entry) &key time (root *root*))
  (declare (ignore time) (type (or string pathname null) root))
  (with-slots (file) entry
    (when root ;Change the root.
      (setf file (from-path-root
		  (concatenate 'string (file root) (file entry))
		  (root log))))
    (setf (get-entry log file) entry)))

(defmethod add-entry ((log base-log) (add-list list) 
		      &key (time (get-universal-time)) (root *root*))
  (dolist (add add-list)
    (add-entry log add :time time :root root)))

(defmethod add-entry ((log base-log) (add (eql :new-files))
		      &key (time (get-universal-time)) (root *root*))
  (declare (type integer time))
  (dolist (path (cl-fad:list-directory *default-pathname-defaults*))
    (add-entry log (path-stuff:from-path-root (file path))
	       :time time :root root)))

(defmethod add-entry
    ((log base-log) (add-log base-log)
     &key (time (get-universal-time)) (root (slot-value add-log 'root)))
  (map-entries add-log (entry)
    (add-entry log entry :time time :root root)))

(defmacro add-entry-hook
    (((log log-type) entry &key (time `(time (get-universal-time))))
     &body body)
  "'Hook' for adding entry of a particular type of log. This way you can\
 for instance read a bit from the file, change the class."
  (with-gensyms (path)
    `(progn
       (defmethod add-entry :around ((,log ,log-type) (,path string)
				     &key ,time (root *root*))
	 (let ((,entry (call-next-method ,log ,path
					 :time time :root root)))
	   ,@body))
       (defmethod add-entry ((,log ,log-type) (,path pathname)
			     &key ,time (root *root))
	 (add-entry ,log (file ,path) :time time :root root)))))
	
(defgeneric read-log (from)
  (:documentation "Reads a log."))

(defmethod read-log ((log base-log))
  (when (probe-file (file log))
    (add-entry log (restore (file log))))
  log)

(defmethod read-log ((file string))
  (read-log (pathname file)))

(defmethod read-log ((file pathname))
  (if (probe-file file)
    (restore file)
    (make-instance 'base-log :file file)))

(defun with-log-fn (read-from fn &key (log (read-log read-from)))
  (let ((*root* (slot-value log 'root)))
    (prog1 (funcall fn log)
      (store log (file read-from)))))

(defmacro with-log ((&optional (log-var (gensym)) read-from) &body body)
  "Reads a log, allows you to do body, then writes it again.
The if LOG argument 1 element, just a filename witht the log, or a log to\
 read from. Otherwise, log is taken to be arguments to make-instance."
  `(with-log-fn ,read-from (lambda (,log-var) ,@body)))

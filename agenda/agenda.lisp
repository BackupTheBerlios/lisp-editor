
(cl:in-package :cl-user)

(defpackage :log
  (:use :common-lisp :generic :denest
	:gil :gil-share)
  (:export add-entry entry))

(in-package :log)

(defvar *directory* nil)

(defvar *just-read-statistics* nil
  "Just read log data, not log entries.")

(defvar *entry-level* 1)

(defvar *entries* nil)

(defmacro read-entry-args (args &body body)
  "Turns entry argument list into variables with their elements."
  `(destructuring-bind
	 (entry-name name &key keywords links sub-entry status) ,args
     (declare (ignorable name entry-name keywords links sub-entry status))
     ,@body))

(defun entry (&rest entry-args)
  "Creates an entry.\
 (gil:*lang* must be set if you are not *just-read-statistics*)"
  (push entry-args *entries*)
  (unless *just-read-statistics*
    (read-entry-args entry-args
      (section *entry-level* name name
	       (gil-read:gil-execute (format nil "~D.gil" entry-name))))))

(defun load-entries-file (entries-file)
  (let ((*just-read-statistics* t))
    (load entries-file)))

(defvar *last-log-time* (get-universal-time))
(defvar *this-time-cnt* 0)

(defun gen-entry-name (time)
  (multiple-value-bind
	(second minute hour date month year day daylight-p zone)
      (decode-universal-time time)
    (declare (ignore date daylight-p zone))
    (format nil "entry_~D:~D:~D_on_~D-~D-~D~D" 
	    hour minute second  day month year
	    (cond ((= *last-log-time* time)
		   (format nil "-~D" (gen:setf- + *this-time-cnt* 1)))
		  (t
		   (setq *this-time-cnt* 0)
		   "")))))

(defun add-entry (entry-args &key gil-fn
		(log-file (format nil "~Dlog.lisp" *directory*))
		log-stream)
  "Adds to a log, entry-args is _without_ entry-name, which is generated \
for you."
  (when (null *directory*)
    (return-from add-entry
      (format t "You need to specify agenda::*directory*~%")))
  (denest
   (let ((entry-name ;Entries named by date, and count if date identity
	  (gen-entry-name (get-universal-time))))
     (denest ;Make gil write gil, and write down entry text.
      (let ((*lang* :gil)))
      (with-open-file (to-stream (format nil "~D~D.gil"
					 *directory* entry-name)
		       :if-does-not-exist :create :direction :output)
	(funcall gil-fn))))
   ;Now write down entry.
   (let ((log-stream*
	  (gen:if-use log-stream (open log-file :direction :output
				       :if-exists :append))))
     (update-entries (cons entry-name entry-args)) ;Update info.
     (format log-stream* "~%(entry ~s~{ ~s~})~%"
	     entry-name entry-args)
     (unless log-stream
       (close log-stream*)))))
   
(defvar *open-entries* t)
(defun open-entries-list ()
  (cond
    ((listp *open-entries*)
     *open-entries*)
    (t
     (setq *open-entries*
	   (remove-if-not (lambda (el)
			    (read-entry-args el
			      (eql status :open)))
			  *entries*)))))

(defun update-entries (entry)
  (read-entry-args entry
    (when (and (listp *open-entries*) (eql status :open))
      (push entry *open-entries*)))
  (push entry *entries*)
  (values))


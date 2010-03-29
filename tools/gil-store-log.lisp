
;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil-store-log
  (:use :common-lisp :alexandria :generic :denest
	:cl-store :store-log :path-stuff
	:gil :gil-share :gil-vars :gil-comms :gil-read :gil-output-util)
  (:export)
  (:documentation ""))

(in-package :gil-store-log)

(defvar *coerce* nil "Whether to coerce stuff into action.")
(declaim (type boolean *coerce*))

(defclass log-action-times ()
  ((last-execute :initarg :last-execute :initform 0 :type integer
		 :reader last-execute)
   (first-rss-write :initarg :last-execute :initform 0 :type integer
		    :reader first-rss-write)
   (last-rss-write :initarg :last-execute :initform 0 :type integer
		   :reader last-rss-write)))

(defclass log-info ()
  ((title :initform nil :accessor log-title)
   (link  :initform nil :accessor log-link)
   (description :initform nil :accessor log-description)
   (notables :initform nil :accessor log-notables)
   (author :initform nil :accessor log-author))
  (:documentation "Overall info"))

(defclass gil-log (base-log log-action-times log-info)
  ((last-rss-change :initarg :last-rss-change :initform -1 :type integer)))

(defclass gil-entry (base-entry log-action-times log-info)
  ())   

(add-entry-hook ((log gil-log) entry)
  (case (intern (file-extension (file entry)) :keyword)
    ((:|.lisp| :|.gil|)
     (change-class entry 'gil-log))
    (t
     entry)))

(defgeneric log-execute (entry when)
  (:documentation "Execute entry or entries."))

(defmethod log-execute ((entry gil-entry) (when (eql t)))
  (with-slots (last-execute had-timestamp) entry
    (setf last-execute (get-universal-time))
    (^let (*lang* :info ;Gather info
	   *most-significant-section* nil ;For title, link, description
	   *notables* nil ;for categories/notables.
	   *author* "")
      (call (execute (file entry)))
      (with-slots (title link description notables author) entry
	(when *most-significant-section*
	  (with-mod-slots "S-" (gils::title gils::name gils::description) 
	      *most-significant-section*
	    (setf title s-title
		  link s-name
		  description s-description)))
	(setf notables *notables* author *author*)))
    (execute (file entry))))

(defmethod log-execute ((entry gil-entry) (when (eql :needed)))
  (with-slots (last-execute had-timestamp) entry
    (when (or *coerce* (< last-execute had-timestamp))
      (log-execute entry t))))

(defmethod log-execute ((log gil-log) when)
  (collecting ()
    (map-entries log (entry)
      (when-let (exec (log-execute entry when))
	(collecting exec)))))

;;RSS stuff.
(defgeneric write-rss (from &key)
  (:documentation "Write rss of something."))

(defmethod write-rss ((log log-info) &key override-notables)
  (with-slots (title link description notables author) log
    (xml-surround "title"
      (^let (*lang* :html) (call title)))
    (xml-surround "link"
      (gil-html:link-url link))
    (xml-surround "category"
      (dolist (n (or override-notables notables))
	(write-string n) (write-string " ")))
    (when author (xml-surround "author" (write-string author)))
    (xml-surround "description"
      (^let (*lang* :html) (call description)))))  

(defmethod write-css :around ((entry gil-entry) &key)
  (xml-surround "item"
    (with-slots (first-rss-change) entry
      (xml-surround "pubDate"
	(write-string (written-time first-rss-change))))
    (call-next-method)))

(defclass rss-output ()
  ((file :initarg :file :initform ".rss" :type string)
   (notables :initarg :notables :initform (list "rss-out") :type list
     :documentation "Which notables it responds to. Defaultly \"rss-out\".
Use these to in-effect make a separate categories for the readers.")
   (backup :initform "" :type string)))

(defun rss-output (file &rest notables)
  (make-instance 'rss-output :file file :notables notables))

(defmethod log-execute ((log gil-log) (output rss-output))
  (denest
   (when (or *coerce* (with-slots (last-rss-write last-rss-change) log
			(< last-rss-write last-rss-change))))
   (with-slots (file backup notables) output
     (when (and (string/= backup "")
		(probe-file file)) ;Make backup if requested.
       (cl-fad:copy-file file (first-nonexistant-path :append-to backup))))
   (^let (list nil)
     ;Gather sorted list of all the entries that qualify.
     (map-entries log (entry)
       (when (subsetp notables (log-notables entry) :test #'string=)
	 (setq list (merge 'list list entry
			   (lambda (a b)
			     (< (first-rss-write a) 
				(first-rss-write b))))))))
  ;Write RSS
   (with-open-file (*standard-output* file :direction :output
		    :if-exists :supersede :if-does-not-exist :create)
     ;Header
     (write-css log :override-notables notables)
     (xml-surround "generator" (write-string "gil-store-log"))
     ;Entries
     (dolist (entry list)
       (write-css entry)))))

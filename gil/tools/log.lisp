;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil-log
  (:use :common-lisp :alexandria :log :path-stuff
	:gil :gil-read :gil-output-util)
  (:export write-rss execute-entry entries-do-if
	   update force-full-redo)
  (:documentation "Extends log system for executing gil files and\
 producing RSS."))

(in-package :gil-log)

(defun later-than (time than-time)
  (if (and time than-time) (> time than-time) :unknown))

(defun past-date (entry of-what)
  "Whether something is past (expiration)date."
  (with-entry-access entry
    (let ((file-date (file-write-date filename))
	  (date (access of-what)))
      (assert file-date nil "How come no file-write-date on ~s" filename)
      (later-than file-date date))))

(defun doesnt-have (entry what)
  "Sees if an entry has a particular element."
  (with-entry-access entry (not (access what))))

(defun entries-do-if (action &optional (predicate :all))
  "Does action on entry if predicate, also takes keywords:
Keywords :all(default), :need-execute (changed after previous execution),
 :never-executed (never executed at all), :changed; timestep later than\
 denoted."
  (let ((predicate
	 (case predicate
	   (:never-executed
	    (rcurry #'doesnt-have :last-execute-time))
	   (:all
	    (gen:constant t))
	   (:changed
	    #'log:entry-changed-p)
	   (:needs-execute
	    (rcurry #'past-date :last-execute-time))
	   (:needs-update
	    (rcurry #'past-date :last-update-time))
	   (t
	    (assert (not(keywordp predicate)) nil 
		    "Keyword ~s not recognized." predicate)
	    predicate))))
    (declare (type (function (list) (values)) predicate))
    (mapcan (lambda (entry)
	      (when (funcall predicate entry)
		(list (funcall action entry))))
	    *log-entries*)))

(defun rss-file ()
  (or (log-data :rss-file) ".rss"))

(defun gillable (filename)
  (case (intern (file-extension filename) :keyword)
    ((:|.gil| :|.lisp|) t)))

(defun update-entry (entry &key (time (get-universal-time)))
  "Update entry."
  (setf (log-data :last-update-time) time)
  (with-entry-access entry
    (setf (access :last-update-time) time)
(defun written-time (ut) ;TODO move somewhere else.
  "Time, but written out, hopefullying suiting rss."
  (multiple-value-bind
	(second minute hour date month year day daylight-p zone)
      (decode-universal-time ut 0)
    (declare (ignore daylight-p zone))
    (flet ((two-digit (n)
	     (format nil (cond ((< n 10) "0~a") ((< n 100) "~a")) n)))
      (format nil "~a, ~a ~a ~a, ~a:~a:~a UT"
	 (aref (vector "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day)
	 date
	 (aref (vector "Jan" "Feb" "Mar" "Apr" "May" "Jun"
		       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") month)
	 year
	 (two-digit hour) (two-digit minute) (two-digit second)))))
    (unless (access :first-update-time)
      (setf (access :first-update-time) time))
    (let ((gil:*lang* :info) ;Gather info.
	  (gil-info:*contents* nil)
	  (gil-info:*enclosure* nil)
;	  (gil-info:*attribute* nil)
	  (gil-info:*comments-thread* nil)
	  (gil-info:*notables* nil)
	  (gil-info:*most-significant-section* nil))
      (let ((*package* (find-package :gil-user)))
	(call (execute filename))) ;Gathering
      
      ;Conversion to outputs of rss is delayed so can be done with :info
      ;first. :section should never be written to disk.
      (setf (access :section) gil-info:*most-significant-section*)
      
      (setf (access :notables)
	    (mapcar (lambda (notable) 
		      (with-output-to-string (*standard-output*)
			(let ((gil:*lang* :txt)) (call notable))))
		    gil-info:*notables*))
      
      (when gil-vars:*author*
	(setf (access :author) gil-vars:*author*))
      (when gil-info:*comments-thread*
	(setf (access :comments-thread-name) gil-info:*comments-thread*
	      (access :comments-thread)
	      (gil-html:link-url gil-info:*comments-thread*))))))


(defun write-str (*lang* &rest objects)
  (with-output-to-string (*standard-output*)
    (call-list objects)))

(defun write-rss-elements (entry section)
  (with-entry-access entry
    (with-slots (gils::name gils::title gils::description) section
      (setf (access :link-sym) gils::name
	    (access :link) (gil-html:link-url gils::name)
	    (access :title) (write-str :html gils::title))
      (when gils::description
	(setf (access :description) (write-str :html gils::description))))
    (setf (access :section) nil))) ;Must be nil before write!

(defun write-rss-entry (entry)
  "Write single entry into rss."
  (with-entry-access entry
    (unless (access :first-rss-write)
      (setf (access :first-rss-write) (get-universal-time)
	    (log-data :last-rss-change) (get-universal-time)))
    (when (gillable filename)
      (xml-surround "item"
	(xml-surround "pubDate"
	  (write-string (written-time (access :first-rss-write))))
	
	(when-let (section (access :section)) ;If needed, fill entries 
	  (write-rss-elements entry section)) ;for below.
	
	(mapcar (lambda (of)
		  (when-let (got (access of))
		    (xml-surround (string-downcase of)
		      (write-string got))))
		'(:title :description :author :link :comments))
	
	(dolist (notable (access :notables))
	  (xml-surround "category" (write-string notable)))))))

(defun write-rss (&key (backup t))
  "Update rss file."
  (setf (log-data :last-rss-write) (get-universal-time))
  (when (and backup (probe-file (rss-file)))
    (cl-fad:copy-file (rss-file) (format nil "~a-backup" (rss-file))
		      :overwrite t))
  (with-open-file (*standard-output* (rss-file) :direction :output
		   :if-exists :supersede :if-does-not-exist :create)
    (xml-surround "title"
      (if-let (title (log-data :log-title))
	(write-string title)
	(warn "No title for RSS!")))
    (xml-surround "link"
      (write-string (or (log-data :log-url-link)
			(error "Tried to write rss without link!"))))
    (xml-surround "description"
      (when-let (description (log-data :log-description))
	(write-string description)))
    (xml-surround "pubDate"
      (write-string (written-time (or (log-data :last-rss-change)
				      (log-data :last-addition)))))
    (dolist (category (log-data :categories))
      (xml-surround "category" (write-string category)))
    (xml-surround "generator" (write-string "gil-log"))
    (entries-do-if
     (lambda (entry)
       (with-entry-access entry ;Executed entries have rss element.
	 (unless (access :first-update-time)
	   (update-entry entry))
	(write-rss-entry entry))))))

(defun execute-entry (entry)
  "Executes an entry. Adds rss mention if needed."
  (with-entry-access entry ;Executed entries have rss element.
    (when (gillable filename)
      (unless (later-than (access :last-update-time)
			  (file-write-date filename))
	(update-entry entry))
      (setf (access :last-execute-time) (get-universal-time))
      (execute filename))))

;TODO showing them chained.
(defun update (&key (for :all))
  "Updates everything regardless of it needs to be. 
Hopefully won't be used."
  (prog1 (entries-do-if #'execute-entry for)
    (write-rss)))

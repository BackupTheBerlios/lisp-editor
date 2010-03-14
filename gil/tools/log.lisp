;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil-log
  (:use :common-lisp :generic :log
	:gil :gil-read :gil-output-util)
  (:export write-rss execute-entry execute-entries-if)
  (:documentation "Extends log system for executing gil files and\
 producing RSS."))

(in-package :gil-log)

(defun past-date (entry of-what)
  "Whether something is past (expiration)date."
  (with-entry-access entry
    (if (access of-what)
      (> (file-write-date filename) (access of-what)) t)))

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
	    (curry #'doesnt-have :last-execute-time))
	   (:all
	    (constant t))
	   (:changed
	    #'log:entry-changed-p)
	   (:needs-execute
	    (curry #'past-date :last-execute-time))
	   (:needs-update
	    (curry #'past-date :last-update-time))
	   (t
	    (assert (not(keywordp predicate)) nil 
		    "Keyword ~s not recognized." predicate)
	    predicate))))
    (declare (type (function (list) (values)) predicate))
    (dolist (entry *log-entries*)
      (when (funcall predicate entry)
	(funcall action entry)))))

(defun rss-file ()
  (or (log-data :rss-file) ".rss"))

(defun update-entry (entry &key (time (get-universal-time)))
  "Update entry."
  (setf (log-data :last-update-time) time)
  (with-entry-access entry
    (setf (access :last-update-time) time)
    (unless (access :first-update-time)
      (setf (access :first-update-time) time))
    (let ((gil:*lang* :info) ;Gather info.
	  (gil-info:*contents* nil)
	  (gil-info:*enclosure* nil)
;	  (gil-info:*attribute* nil)
	  (gil-info:*comments-thread* nil)
	  (gil-info:*notables* nil))
      (call (execute-entry entry)) ;Gathering
      (with-slots (gils::level gils::name gils::title gils::description)
	  (car gil-info:*contents*)
	(setf (access :title)
	      (with-output-to-string (*standard-output*)
		(let ((gil:*lang* :html)) (call gils::title)))
	      (access :link-name) gils::name
	      (access :link) (gil-html:cur-link-url gils::name))
	(unless (string= gils::description "")
	  (setf (access :description) gils::description)))
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
	      (gil-html:cur-link-url gil-info:*comments-thread*))))))

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

(defun write-rss-entry (entry)
  "Write single entry into rss."
  (with-entry-access entry
    (unless (access :first-rss-write)
      (setf (access :first-rss-write) (get-universal-time)
	    (log-data :last-rss-change) (get-universal-time)))
    (xml-surround "item"
      (xml-surround "pubDate"
	(write-string (written-time (access :first-rss-write))))
      (mapcar (lambda (keyword)
		(when (access keyword)
		  (xml-surround (string-downcase keyword)
		    (write-string (access keyword)))))
	      '(:title :link :author :description :comments-thread))
      (dolist (notable (access :notables))
	(xml-surround "category" (write-string notable))))))

(defun write-rss (&key (backup t))
  "Update rss file."
  (setf (log-data :last-rss-write) (get-universal-time))
  (when backup
    (cl-fad:copy-file (rss-file) (format t "~a-backup" (rss-file))))
  (with-open-file (*standard-output* (rss-file))
    (xml-surround "title"
      (if-let title (log-data :log-title)
	(write-string title)
	(warn "No title for RSS!")))
    (xml-surround "link"
      (write-string (or (log-data :log-link)
			(error "Tried to write rss without link!"))))
    (xml-surround "description"
      (when-let description (log-data :log-description)
	(write-string description)))
    (xml-surround "pubDate"
      (write-string (written-time (log-data :last-rss-change))))
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
    (when (< (access :last-update-time) (file-write-date filename))
      (update-entry entry))
    (setf (access :last-execute-time) (get-universal-time))
    (xml-surround "generator" (write-string "gil-log"))
    (execute filename)))

;TODO showing them chained.

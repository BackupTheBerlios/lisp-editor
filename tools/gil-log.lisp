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
  (:use :common-lisp :log :gil-read :gil-share)
  (:export gil-entries
	   read-new-entries execute-entry
	   execute-entries-if execute-new-entries execute-all-entries)
  (:documentation "Takes log, and puts some stuff around it.
keyword :gil means that the file can and is meant be gil-executed. 
  (So a .gil or a .lisp meant as gil file.)
Qualifier :gil-dep, is to mean something depends on another,\
 but not actually used.."))

(in-package :gil-log)

(defun gil-entries (&key (condition (lambda (entry)
				      (declare (ignore entry)) t)))
  "Gets all the entries with gil as keyword."
  (remove-if-not (lambda (entry)
		   (and (find :gil (keywords entry))
			(funcall condition entry)))
		 (copy-list *entries*)))

(defun read-new-entries (dirname)
  "Reads new entries in a directory."
  (add-directory-entries :dirname dirname :hook
    (lambda (entry)
      (let ((read (with-open-file (stream (file entry)) (read stream))))
	(if (eql (car read) 'about-file)
	  (destructuring-bind
		(name &key keywords links) (getf :log (cdr read))
	    (change-class entry 'log::log-entry
	      :entry-name name :keywords keywords :links links
	      :entry-time (eval entry-time) :data data))
	  entry)))))

(defun execute-entry (entry)
  "Executes an entry."
  (let ((gils::*timestamp* (entry-time entry)))
    (execute (file entry))))

(flet ((entry-new-p (entry)
	 (not (getf (slot-value entry 'log::data) :execute-time)))
       (true (entry) (declare (ignore entry))))
  (defun execute-entries-if
      (&optional (predicate #'entry-new-p))
    "Executes entries as needed, default is new."
    (dotimes (entry *entries*)
      (when (funcall if entry)
	(change-class entry 'log::log-entry
	   :data (append (slot-value entry 'log::data)
			 (list :execute-time (get-universal-time))))
	(execute-entry entry))))
  (defun execute-new-entries ()
    "Executes new entries."
    (execute-entries-if))
  (defun execute-all-entries ()
    "Executes all entries."
    (execute-entries-if #'true)))

;; TODO automatically:
;; * find new entries in a folder. (with cl-fad)
;; * .rss output with them.
;; * List for them. (Same/similar system as for contents? Generalize it?)

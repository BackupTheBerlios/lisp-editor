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
  (:use :common-lisp :log :gil-read)
  (:documentation "Takes log, and puts some stuff around it.
keyword :gil means that the file can and is meant be gil-executed. 
  (So a .gil or a .lisp meant as gil file.)
Qualifier :gil-dep, is to mean something depends on another,\
 but not actually used.."))

(in-package :gil-log)

(defun execute-gil-entry (entry)
  (lambda ()
    (let ((gils::*timestamp* (entry-time entry)))
      (funcall (execute (file entry))))))

(defun gil-entries (&key (condition (lambda (entry)
				      (declare (ignore entry)) t)))
  "Gets all the gil-entries."
  (remove-if-not (lambda (entry)
		   (and (find :gil (keywords entry))
			(funcall condition entry)))
		 (copy-list *entries*)))

;; TODO automatically:
;; * find new entries in a folder. (with cl-fad)
;; * .rss output with them.
;; * List for them. (Same/similar system as for contents? Generalize it?)

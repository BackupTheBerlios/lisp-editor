;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil-user
  (:use :common-lisp :generic :denest
	:gil :gils :gil-style :gil-read)
  (:export run-gil handle-page-contents)
  (:documentation "Package for the user of gil.."))

(in-package :gil-user)

(defun handle-page-contents
    (contents &key 
     (split (mk-split (list 0.2 0.8)))
     (enlist nil)
     (before (lambda())) (after (lambda()))
     number (number-upto (if number 4 0))
     (include-upto 3) (via :header) replace-names)
  "Makes page handler like for into gils::*handle-page*, for attaching\
 content listing all the sections to the left. (most arguments are\
 identical to use-contents.)"
  (lambda (page)
    (let ((contents-side
	   (glist :series
		  before 
		  (gil-info:use-contents contents
		   :number number :number-upto number-upto :number number
		   :include-upto include-upto :via via
		   :replace-names replace-names)
		  after)))
      (if enlist (glist split (list contents-side page))
	         (glist split contents-side page)))))

(defun run-gil
    (from main-output
     &key (lang :html)
     (to-path *default-pathname-defaults*)
     saved-links saved-contents)
  "Processes GIL files for you, many special variables served as\
 keyword arguments for you."
  (denest:denest
   (let ((*default-pathname-defaults* to-path)))
   (with-open-file (*standard-output* main-output :direction :output
		    :if-does-not-exist :create :if-exists :supersede))
   (let*((*lang* lang)
	 (gil-info::*links*
	  (if-use saved-links (make-hash-table)))
	 (gil-info::*contents*
	  saved-contents))
     (assert (if saved-links saved-contents (not saved-contents))
	     nil "If you provide saved links also provide saved contents.")
     (unless saved-links ;TODO seems to be last bit not working.
       (gil-info:gather from))
     (values
      (funcall
	(funcall gils::*handle-page* (execute from)))
      saved-links saved-contents))))

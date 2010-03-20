
;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil-user
  (:use :common-lisp :alexandria :denest
	:gil :gil-vars :gil-share :gil-style :gil-read)
  (:export run-gil side-paned-page-handler)
  (:documentation "Package for the user of gil.."))

(in-package :gil-user)

(defun side-paned-page-handler
    (&key top-pane left-pane
          (top-pane-args '(:colspan 2))
          (left-pane-args '(:valign :top :width "20%"))
          (page-pane-args '(:valign :top)))
  "Stuff for basic webpage, with banner ontop, summary to the left, and\
 some stuff above/below that. (default contents treatment is that\
 contents:use-contents does. (gil-info:gather-contents and 
gil-contents:use-contents can help make your contents based on sections 
you used.)"
  (lambda (page)
    (table (when top-pane (list (table-el top-pane-args top-pane)))
	   (list (table-el left-pane-args left-pane)
		 (table-el page-pane-args page)))))

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
	  (or saved-links (make-hash-table)))
	 (gil-info::*contents*
	  saved-contents))
     (assert (if saved-links saved-contents (not saved-contents))
	     nil "If you provide saved links also provide saved contents.")
     (unless saved-links ;TODO seems to be last bit not working.
       (gil-info:gather from))
     (values
      (funcall
	(funcall *handle-page* (execute from)))
      saved-links saved-contents))))

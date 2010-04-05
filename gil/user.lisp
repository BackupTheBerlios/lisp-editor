
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
  (:export side-paned-page-handler)
  (:documentation "Package for the user of gil.."))

(in-package :gil-user)

(defun side-paned-page-handler
    (&key top-pane left-pane
          (top-pane-args '(:colspan 2))
          (left-pane-args '(:valign :top :width "20%"))
          (page-pane-args '(:valign :top))
          (with-header t))
  "Stuff for basic webpage, with banner ontop, summary to the left, and\
 some stuff above/below that. (default contents treatment is that\
 contents:use-contents does. (gil-info:gather-contents and 
gil-contents:use-contents can help make your contents based on sections 
you used.)"
  (lambda (name header-result objects)
    (declare (ignore name))
    (table (when top-pane (list (table-el top-pane-args top-pane)))
	   (list (table-el left-pane-args left-pane)
		 (table-el page-pane-args
		  (glist-list :series (if with-header
					(cons header-result objects)
					objects)))))))

;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil-info
  (:use :common-lisp :generic :denest :gil :gil-share :gil-read)
  (:export use-contents gather gather-contents
	   link-to-url)
  (:documentation
   "Contains gathering data for contents, and creation of contents section.
Gathering contents also registers and handles links if not already\
 registered."))

(in-package :gil-info)

(defvar *contents* nil)
(defvar *links* (make-hash-table))
(defclass link-entry ()
  ((page :initarg :page)))
(defun find-link-page (link-name)
  (gethash (gils::intern* link-name) *links*))

(defvar *register-first* nil)

(def-call (fun function) (funcall fun))
(def-call (str string) str)
(def-call (num number) num)
(def-call (symbol symbol) symbol)
(def-call anything (error "Not recognized ~a" anything))
(def-call (null null) (declare (ignore null)))

(defun gather (from)
  "Collects table of contents from files."
  (unless (eql *lang* :info)
    (let ((*lang* :info)
	  (gils::*handle-page* #'identity))
      (call (gil-read::execute from)))
    (setq *contents* (reverse *contents*))))

(defun gather-contents (from)
  "Gather, but isolates and returns gil:*contents* for you."
  (let ((gil-info::*contents* nil))
    (gather from)
    gil-info::*contents*))

(defun register-link (link-name)
  (let ((link-sym (gils::intern* link-name)))
    (typecase (gethash link-sym *links*)
      (link-entry
       (warn "Position that can be linked to created twice. 
Links might go not as intended! Link name: ~a
Page ~a changed to page ~a." link-sym
        (slot-value (gethash link-sym *links*) 'page) gils::*cur-page*)))
    (setf (gethash link-sym *links*) 
	  (make-instance 'link-entry :page gils::*cur-page*))))

(def-glist anything objects
  (declare (ignore anything))
  (mapcar #'call objects))

(def-glist (link gils::link) objects
  (register-link (gils::name link))
  (mapcar #'call objects))

(def-glist (link gils::follow-link) objects
  (declare (ignore link))
;  (register-link (gils::name link))
  (mapcar #'call objects))

(def-glist (url-link gils::url-link) objects
  (declare (ignore url-link))
  (mapcar #'call objects))

(def-glist (section section) list
  (with-slots (gils::level gils::name gils::title) section
    ;TODO in big doodoo, i need title for _different language_
    (push (list gils::level gils::name gils::title 
		(when *register-first* (car list)))
	  *contents*)
    (cond
      ((> gils::level gils::*section-page-level*)
       (register-link gils::name)
       (call gils::title)
       (mapcar #'call list))
      (t ;Produces new page.
       (let ((gils::*cur-page*; (gils::intern* gils::name))
	      (if-let path (gethash gils::name gils::*page-path*)
	        (format nil "~a~a" path gils::name) ;Path specified.
		gils::name)))
	 (register-link gils::name)
	 (call gils::title)
	 (mapcar #'call list))))))

(def-call (table-el table-el)
  (call-list (slot-value table-el 'gils::contents)))

(def-glist (table gils::table) contents
  (mapcar (lambda (el)
	    (if (listp el)
		(mapcar #'call el)
		(call el)))
	  contents))

(defclass url-entry ()
  ((url :initarg :url :type string)))
(defun link-to-url (link-name url)
  "Links a link-name to an (arbitrary)url."
  (setf (gethash (gils::intern* link-name) *links*)
	(make-instance 'url-entry :url url)))

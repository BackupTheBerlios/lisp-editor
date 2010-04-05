;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil-info
  (:use :common-lisp :alexandria
	:denest :gil :gil-vars :gil-comms :gil-share :gil-read)
  (:export use-contents gather gather-contents
	   link-to-url)
  (:documentation
   "Contains gathering data for contents, and creation of contents section.
Gathering contents also registers and handles links if not already\
 registered."))

(in-package :gil-info)

(defclass link-entry ()
  ((page :initarg :page :type page :reader link-page)))

(defmethod page-directory ((entry link-entry))
  (page-directory (link-page entry)))

;;Link stuff

(defun register-link (link-name)
  "Registers a link to be at some location."
  (when link-name
    (typecase (get-link link-name)
      (link-entry
       (warn "Position that can be linked to created twice. 
Links might go not as intended! Link name: ~a
Page ~a changed to page ~a."
	     link-name (slot-value (get-link link-name) 'page)
	     *cur-page*)))
    (setf (get-link link-name)
	  (make-instance 'link-entry :page *cur-page*))))
;;End link stuff.

(gil::basic-lang :info)

(def-call (str string) str)
(def-call (num number) num)
(def-call (symbol symbol) symbol)
(def-call (null null) (declare (ignore null)))

(defun gather (from)
  "Collects table of contents from files."
  (unless (eql *lang* :info)
    (let ((*lang* :info)
	  (*handle-page* #'identity))
      (call (gil-read::execute from)))
    (setq *contents* (reverse *contents*))))

(defun gather-contents (from)
  "Gather, but isolates and returns gil:*contents* for you."
  (let ((gil-info::*contents* nil))
    (gather from)
    gil-info::*contents*))

(def-glist anything objects
  (declare (ignore anything))
  (call-list objects))

(def-glist :comment objects
  "Comment is useful also because info doesn't ignore it."
  (call-list objects))

(def-glist (link link) objects
  (register-link (gils::name link))
  (call-list objects))

(def-glist (link follow-link) objects
  (declare (ignore link))
;  (register-link (gils::name link))
  (call-list objects))

(def-glist (url-link url-link) objects
  (declare (ignore url-link))
  (call-list objects))

(def-glist (section section) list
  (push section *contents*)
  (with-slots (gils::name gils::level gils::title) section
    (when (or (not *most-significant-section*)
	      (< gils::level ;;Keep track of which section is most significant.
		 (slot-value *most-significant-section* 'gils::level)))
      (setq *most-significant-section* section))
    (let ((*cur-pos* gils::name))
      (cond
	((> gils::level *section-page-level*)
	 (register-link gils::name)
	 (call gils::title)
	 (call-list list))
	(t ;Produces new page.
	 (let ((*cur-page* (get-page gils::name)))
	   (register-link gils::name)
	   (call gils::title)
	   (call-list list)))))))

(def-call (table-el table-el)
  (call-list (slot-value table-el 'gils::contents)))

(def-glist (table table) contents
  (mapcar (lambda (el)
	    (if (listp el)
	      (call-list el) (call el)))
	  contents))

(defclass url-entry ()
  ((url :initarg :url :type string)))
(defun link-to-url (link-name url)
  "Links a link-name to an (arbitrary)url."
  (setf (get-link link-name) (make-instance 'url-entry :url url)))

;;Listing notables.
(defclass notable ()
  ((pos :initarg :pos :type (or string symbol))
   (objects :initarg :objects :type list)
   (str :initform "" :type string)))

(defmethod notable-str ((notable notable))
  (with-slots (str objects) notable
    (if (= (length str) 0)
      (let ((*lang* :txt))
	(setf str (with-output-to-string (*standard-output*)
		    (call (glist-list :series objects)))))
      str)))

(def-glist :notable objects
  (push (make-instance 'notable :pos *cur-pos* :objects objects)
	*notables*)
  (call-list objects))



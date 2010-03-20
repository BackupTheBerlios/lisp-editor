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
	:denest :gil :gil-vars :gil-share :gil-read)
  (:export use-contents gather gather-contents
	   link-to-url
	   *contents* *notables* *enclosure* *comments-thread*)
  (:documentation
   "Contains gathering data for contents, and creation of contents section.
Gathering contents also registers and handles links if not already\
 registered."))

(in-package :gil-info)

(defvar *contents* nil)

(defclass link-entry () ((page :initarg :page)))

(defvar *register-first* nil)

;;Link stuff
(defvar *links* (make-hash-table))

(defun link-to-keyword (link-name)
  (intern (format nil "~a" link-name) :keyword))

(defun get-link-page (link-name)
  (gethash (link-to-keyword link-name) *links*))
(defun (setf get-link-page) (to link-name)
  (setf (gethash (link-to-keyword link-name) *links*) to))

(defun register-link (link-name)
  (when link-name
    (typecase (get-link-page link-name)
      (link-entry
       (warn "Position that can be linked to created twice. 
Links might go not as intended! Link name: ~a
Page ~a changed to page ~a."
	     link-name (slot-value (get-link-page link-name) 'page)
	     gils::*cur-page*)))
    (setf (get-link-page link-name)
	  (make-instance 'link-entry :page gils::*cur-page*))))
;;End link stuff.

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
  (push section *contents*)
  (with-slots (gils::name gils::level gils::title) section
    (let ((*cur-pos* gils::name))
      (cond
	((> gils::level gils::*section-page-level*)
	 (register-link gils::name)
	 (call gils::title)
	 (mapcar #'call list))
	(t ;Produces new page.
	 (let ((gils::*cur-page*; (gils::intern* gils::name))
		(if-let (path (gethash gils::name gils::*page-path*))
		  (format nil "~a~a" path gils::name) ;Path specified.
		  gils::name)))
	   (register-link gils::name)
	   (call gils::title)
	   (mapcar #'call list)))))))

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
  (setf (get-link-page link-name) (make-instance 'url-entry :url url)))

;;Listing notables.
(defvar *notables* nil
  "List of things stated notable.(With link names to them.")

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

(defvar *comments-thread* nil
  "Denotes a link to the comments thread if any found.")
(defvar *enclosure* nil)

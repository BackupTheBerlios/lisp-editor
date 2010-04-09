;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil-comms
  (:use :common-lisp :generic :alexandria :gil :gil-vars)
  (:export
   *pages* find-page get-page *cur-page*
   page page-name page-nr page-directory
   *links* get-link
   link-entry url-entry link-page; link-url ;TODO probably bring them here.
   *dead-links* *double-links*
   *cur-pos*
   *contents* *enclosure* *comments-thread* *most-significant-section*
   *notables*
   
   *cur-char-depth*)
  (:documentation "Various variables communicating from :info and/or\
 such. Not at the user end."))

(in-package :gil-comms)

;;Internal communication inside packages.(Suggested.)
(defclass page ()
  ((name :initarg :name :type symbol :reader page-name)
   (directory :initarg :directory :type string :reader page-directory)
   (page-nr :initarg :page-nr :type fixnum :reader page-nr)))

(defvar *pages* nil "List of pages.")

(defvar *page-nr* 0 "Max page number.")

(defun find-page (name)
  (find-if (lambda (page)
	     (eql (page-name page) name)) *pages*))

(defun get-page (name)
  "Gets a particular page, creating it if needed."
  (or (find-page name)
      (prog1 (car (push (make-instance 'page :name name
			  :page-nr *page-nr*
			  :directory *following-directory*)
			*pages*))
	(setf- + *page-nr* 1))))

(defvar *cur-page* nil
  "Suggested variable for communication of current page.")
(defvar *cur-pos* ""
  "Suggested variable of communication of how one would link\
 to the current position.")
(defvar *cur-char-depth* 0
  "Suggested variable of communication of Current character depth.")

(defvar *links* (make-hash-table)
  "Keeps track of links.")

(defun get-link (link-name)
  (gethash link-name *links*))
(defun (setf get-link) (to link-name)
  (setf (gethash link-name *links*) to))

(defclass link-entry ()
  ((page :initarg :page :type page :reader link-page)))

(defmethod page-directory ((entry link-entry))
  (page-directory (link-page entry)))

(defclass url-entry ()
  ((url :initarg :url :type string)))

(defvar *dead-links* nil
  "Links that failed to have an end to it.")
(defvar *double-links* nil
  "Links that have more than one endpoint.")

(defvar *contents* nil
  "Section info for page of contents is gathered here.")

(defvar *comments-thread* nil
  "Denotes a link to the comments thread if any found.")
(defvar *enclosure* nil)

(defvar *most-significant-section* nil
  "Most important section read so far. This way you can figure out a name\
 for a bit of code by seeing what the most significant section of a part\
 is.")

(defvar *notables* nil
  "List of things stated notable.(With link names to them.")

(declaim (type hash-table *links*)
	 (type list *dead-links* *double-links* *contents* *notables*))

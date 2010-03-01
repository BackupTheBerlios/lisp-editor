;;
;;  Copyright (C) 2010-03-01 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil-style
  (:use :common-lisp :generic :gil)
  (:export register-style refer-style inline-style
	   *styles* style-list
	   write-style-css write-prop-val-css)
  (:documentation "Style system, mirroring CSS."))

(in-package :gil-style)

;;Styles based on CSS, and mirrors it. (why not)

(defvar *styles* nil)

;TODO *attempt-readable*

(defun write-prop-val-css (code stream)
  (dolist (pv code)
    (destructuring-bind (prop val &rest rest) pv
      (assert (null rest) nil
	      "property, value, no more. got ~a" rest)
      (format stream "~a:~a;" prop val))))

(defun write-style-css (code &optional stream)
  (dolist (el code)
    (destructuring-bind
	  (element-type (&optional (name '|nil|) (manners '|nil|))
			&rest prop-val-pairs) el
      (format stream "~a" element-type)
      (unless (string= (string-downcase (symbol-name name)) "nil")
	(format stream "~a." name))
      (when (if (listp manners) manners
		(not(string= (string-downcase (symbol-name manners))
			     "nil")))
	(format stream "~{:~a~}"
		(if (listp manners) manners (list manners))))
      (format stream "{")
      (write-prop-val-css prop-val-pairs stream)
      (format stream "}"))))

(defclass style-list ()
  ((list :initarg :list :type list)))

(defun read-from-string-case-sensitive (str)
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :preserve)
    (read-from-string str)))

;Styles read with just READ(-FROM-STRING)
(defun register-style (&rest str)
  "Registers a bunch of styles. Each style goes 
 (element-type (&optional name manners) &rest prop-val-pairs)
 Symbols that 'css would recognize' are done as css."
  (let ((styles (read-from-string-case-sensitive
		 (format nil "(~{~a~})" str))))
    (setf- append *styles* styles) ;Add styles.
    (glist (make-instance 'style-list :list styles))))
  
;;Refer to them/have them inline.
(defclass refer-style ()
  ((refer-to :initarg :refer-to :type string))
  (:documentation "Refer to from CSS style."))

;TODO: needs string as argument; problem is that i need it case sensitive.
(defun refer-style (style &rest objects)
  (glist-list (mk refer-style :refer-to style) objects))

(defclass inline-style ()
  ((style :initarg :style :type list))
  (:documentation "in-line style."))

(defun inline-style (style-code &rest objects)
  (glist-list (mk inline-style
		  :style (read-from-string-case-sensitive
			  (format nil "(~a)" style-code)))
	      objects))

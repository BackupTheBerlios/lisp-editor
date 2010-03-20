;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil-style
  (:use :common-lisp :generic :gil)
  (:export register-style refer-style inline-style style refer
	   *styles*
	   write-style-css write-prop-val-css)
  (:documentation "Style system, using CSS. (In future might want to\
 expand on that."))

(in-package :gil-style)

;;Styles based on CSS, and mirrors it. (why not)

(defvar *styles* nil)

;TODO *attempt-readable*

;Styles read with just READ(-FROM-STRING)
(defun register-style (&rest styles)
  "Registers a bunch of styles. Each style goes 
 (element-type (&optional name manners) &rest prop-val-pairs)
 Symbols that 'css would recognize' are done as css."
  (gen:setf- append *styles* styles) ;Add styles.
  (glist-list :style-list styles))

(defmethod i-glist (lang (way (eql :style-list)) (list list))
  (declare (ignore lang list)))

;;Refer to them/have them inline.
(defclass refer-style ()
  ((refer :initarg :refer :type string))
  (:documentation "Refer to from CSS style."))

;TODO: needs string as argument; problem is that i need it case sensitive.
(defun refer-style (style &rest objects)
  (glist-list (mk refer-style :refer style) objects))

(defclass inline-style ()
  ((style :initarg :style :type list))
  (:documentation "in-line style."))

(defun inline-style (style &rest objects)
  (glist-list (mk inline-style :style style)
	      objects))

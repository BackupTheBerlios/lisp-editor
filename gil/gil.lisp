;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil
  (:use :common-lisp :generic)
  (:export mk *lang*
	   cur-def-lang
	   i-glist i-call
	   def-glist def-call
	   glist glist-list
	   call call-list call-list*)
  (:documentation
   "GIL: General Interface _Library_ (But be sure to confuse people with 
the L being Language ;) )

Note that this package does not implement any implementation! 
It only defines functions for it.
Note that you will want to use DENEST with this!

A lot of the defvars are some things implementors can hang on to,
so they're applicable to multiple implementations."))

(in-package :gil)

(defmacro mk (type &rest args)
  "Macro to shorten up make-instance."
  `(make-instance ',type ,@args))

(defvar *lang* nil
  "Current language to convert too.")

(defun cur-def-lang (&key (pkg-name (package-name *package*)))
  "Working on current language."
  (assert (string= (subseq pkg-name 0 4) "GIL-") nil
	  "Demand that packages defining outputs are named GIL-..name..")
  (intern (subseq pkg-name 4) (find-package :keyword)))

;;-------------------List-like objects--------------------------------------

(defgeneric i-glist (lang way things)
  (:documentation "See glist."))

(defmacro def-glist (way objects &body body)
  (with-gensyms (lang wayv)
    `(defmethod i-glist ((,lang (eql ,(cur-def-lang)))
			 ,(if (keywordp way)
			    `(,wayv (eql ,way)) way)
			 (,objects list))
       ,@body)))

(defun glist-list (way things)
  "Lists of various forms.
way:
 :p    Paragraph-like separations
 :list Point-by-point list, class point-list allows for more specification.
 If you made a custom one, and none applies, it reverts to :p"
  (lambda ()
    (i-glist *lang* way things)))

(defun glist (way &rest things)
  (glist-list way things))

(defmethod i-glist ((lang null) way (things list))
  (error "The language is not set."))

(defmethod i-glist (lang way (things list))
  (warn "~a not defined for ~a" (type-of way) lang))

(defmethod i-glist (lang way things)
  (assert (and lang (listp things)) nil
    "~a~a~a." (if lang "" "language not set")
	      (if (not (or lang (listp things))) " and " "")
	      (if (listp things) "" 
		(format nil "last argument must be list, is ~s" things)))
  (error "Don't know what went wrong.
~a ~a ~a ~a" lang way things (type-of things)))

;;Applying

(defgeneric i-call (lang thing)
  (:documentation "Write the stuff."))

(defmacro def-call (object &body body)
  (with-gensyms (lang obj)
    `(defmethod i-call ((,lang (eql ,(cur-def-lang)))
			,(if (keywordp object)
			   `(,obj (eql ,object)) object))
       ,@body)))

(defun call (thing)
  "Does runs i-call with *lang*"
  (i-call *lang* thing))

(defun call-list (list)
  (mapcar #'call list))
(defun call-list* (list)
  (lambda () (call-list list)))

(defmethod i-call ((lang null) thing)
  (error "The language is not set."))

(defmethod i-call (lang (string string))
  (declare (ignore lang))
  (write-string string))

(defmethod i-call (lang (num number))
  (declare (ignore lang))
  (format t "~a" num))

(defmethod i-call (lang (null null))
  (declare (ignore lang null)))

(defmethod i-call (lang (fun function))
  (declare (ignore lang))
  (funcall fun))

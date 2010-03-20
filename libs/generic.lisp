;;Author: Jasper den Ouden
;;This file is in public domain.

(cl:in-package :cl-user)

(defpackage :generic
  (:nicknames :gen)
  (:use :common-lisp :alexandria)
  (:export sqr delist intern*

	   constant
	   
	   for-more setf-
	   case-let when-do

	   mk with-mod-slots
	   with-access with-mod-access)
  (:documentation "Assortment of little useful macros/functions."))

(in-package #:generic)

(defun sqr(x)
  "Square of a value."
  (* x x))

(defun delist (x)
  "If list, return car, otherwise itself."
  (if (listp x) (car x) x))

(defun intern* (x &optional (pkg *package*))
  (if (stringp x) (intern x (or (when (packagep pkg) pkg)
				(find-package pkg)
				:keyword))
      x))

;(defun subseq* (..

(defmacro for-more (macroname &rest args)
  "Applies a series of different arguments to same function."
  (cons 'progn
     (loop for el in args
	collect (cons macroname el))))

(defmacro setf- (operator set &rest args)
  "Changes 'set argument with setf using given operator, and extra\
 arguments. WARNING/TODO: abstraction leak if set has sideeffects."
  `(setf ,set (,operator ,set ,@args)))

;;Conditionals with variables.
(defmacro case-let ((var is) &rest cases)
  "Case, but makes a variable for you."
  `(let ((,var ,is))
     (case ,var ,@cases)))

(defmacro when-do (cond &rest do)
  "return-from the condition, if the condition is true."
  (with-gensyms (s)
    `(when-let (,s ,cond)
       (,@do ,s))))

;;Class/structure stuff.
(defmacro mk (type &rest args)
  "Macro to shorten up make-instance."
  `(make-instance ',type ,@args))

(defmacro with-mod-slots (mod (&rest slots) object &body body)
  "WITH-SLOTS, but requires something to be prepended to the\
 SYMBOL-MACROLET's, this allows you to use two or more objects with\
 convenient symbols at the same time."
  (with-gensyms (obj)
    `(let ((,obj ,object))
       (symbol-macrolet
	   (,@(mapcar (lambda (slot)
			`(,(intern (format nil "~D~D" mod slot))
			   (slot-value ,obj ',slot)))
		      slots))
	 ,@body))))

(defmacro with-mod-access (mod (&rest accessors) object &body body)
  "Access objects. Lists on accessors/readers or plain functions are seen
 as (function &rest args-after) 
Mod adds some name previously so you can work with multiple of the same.\
 (Similar to with-mod-slots.)"
  (with-gensyms (obj)
    `(let ((,obj ,object))
       (symbol-macrolet
	   (,@(mapcar 
	       (lambda (a)
		 `(,(if mod (intern (format nil "~D~D" mod (delist a)))
			    (delist a))
		    (,(delist a) ,obj ,@(when (listp a) (cdr a)))))
	       accessors))
	 ,@body))))

(defmacro with-access ((&rest accessors) object &body body)
  "Access objects. Lists on accessors/readers/ plain functions  are seen as
 (function &rest args-after)."
  `(with-mod-access nil (,@accessors) ,object ,@body))

;;Functions.
(defun constant (value)
  "Produces function with constant value."
  (lambda (&rest rest)
    (declare (ignore rest))
    value))

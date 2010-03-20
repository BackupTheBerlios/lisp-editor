;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl)

(defpackage :example
  (:use :common-lisp :generic)
  (:export arg-example arg-info fun-example 
	   mk-example def-example get-example call-example
	   add-arg-info-example))

(in-package :example)

(defvar *examples* (make-hash-table) "Hash table with the examples.")

(gen:setf-defun get-example (name)
  "Gets the example."
  (gethash name *examples*))

(defclass documented ()
  ((doc :initarg :doc :initform "")))

(defclass arg-info (documented)
  ((type :initform nil :initarg :type
	 :type (or (eql &key) (eql &optional) (eql &rest) null))
   (name :initform nil :initarg :name :type symbol)
   (examples :initform nil :initarg :examples :type list
     :documentation "Examples for possible inputs."))
  (:documentation "Information on argument."))

(defclass fun-example (documented)
  ((name :initarg :name :initform nil :type symbol)
   (args :initarg :args :initform nil :type list)
   (arg-infos :initarg :arg-infos :initform nil :type list
     :documentation "Argument info put together.")
   (body :initarg :body :initform nil :type list)
   (fun :initarg :fun :initform nil :type (or symbol function)))
  (:documentation "(code)Examples for arguments or things in general."))

(defmacro mk-example (name (&rest arguments) &body body)
  (let ((flet-name (intern (format nil "~D-~D" 'example name)))
	(args (gensym))) 
    `(flet ((,flet-name (,@arguments)
	      ,body))
       (let ((,args '(,@arguments)))
	 (make-instance 'fun-example
	   :name ',name :args ,args :body '(,@body)
	   :arg-infos
	   (let (is)
	     (mapcan (lambda (a)
		       (case a
			 ((&key &optional &rest) (setq is a) nil)
			 (t (list (make-instance 'arg-info :name (delist a)
						 :type is)))))
		     '(,@arguments)))
	   :doc ,(when (stringp (car body)) (car body))
	   :fun #',flet-name)))))

(defun call-example (name &rest args)
  "Run example."
  (apply (slot-value (get-example name) 'fun) args))

(defmacro def-example (name (&rest args) &body body)
  `(setf (get-example ',name) (mk-example ,name (,@args) ,@body)))

(defun add-arg-info-example (fun-example arg examples)
  (let ((fun-example (if (symbolp fun-example) (get-example fun-example)
		       fun-example)))
  (setf- append (slot-value (find-if (lambda (el)
				       (eql (slot-value el 'name) arg))
				     (slot-value fun-example 'arg-infos))
			    'examples)
	 (mapcar (lambda (ex)
		   (if (symbolp ex) (get-example ex) ex)) examples))))

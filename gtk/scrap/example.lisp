(cl:in-package :cl)

(defpackage :example
  (:use :common-lisp :generic :denest :gtk-stuff)
  (:export doc-example mk-example def-example))

(in-package :example)

(defvar *examples* (make-hash-table))

(defclass documented ()
  ((doc :initarg :doc :initform "")))

(defclass doc-example (documented)
  ((name :initarg :name :initform nil :type symbol)
   (args :initarg :args :initform nil :type list)
   (arg-examples :initarg :arg-examples :initform nil :type list
     :documentation "More examples, or basic-inputs to create input.")
   (body :initarg :args :initform nil :type list)
   (fun :initarg :fun :initform nil :type (or null function)))
  (:documentation "(code)Examples for arguments or things in general."))

(defmacro mk-example (name (&rest args) &body body)
  (let ((flet-name (intern (format nil "~D-~D" example name))))
    `(flet ((,flet-name (,@args)
	      ,@body))
       (make-instance 'doc-example
	 :name ',name :args '(,@args) :body '(,@body)
	 :doc-str ,(when (stringp (car body)) (car body))
	 :fun #',flet-name))))

(defmacro def-example (name (&args args) &body body)
  `(setf (gethash ',name *examples*) (mk-example ,name (,@args) ,@body)))

(defun exemplary-1-full (arg example)
  (typecase example
    (symbol (make-instance symbol))))

(defun delist (x) (if (listp x) (car x) x))

(defun exemplary-1-short (arg example)
  (typecase example
    (doc-example
     (let ((button (make-instance 'gtk:button
		     :label (string-lowercase (symbol-name (delist arg)))
		     :relief :none)))
       (gobject:g-signal-connect button "clicked"
	 (lambda (b)
	   (declare (ignore b))
	   (
       button)

(defun exemplary-examples (arg examples)
  (let (mem)
    (lambda (b)
      (declare (ignore b))
      (block ex
	(when mem (return-from ex mem))
	(multiple-value-bind (frame box) (frame-box)
	  (gtk:box-pack-start
	   box (defaulted-entry (when (listp a)
				  (format nil "~D" (cadr a)))
		   :entry code-entry))
	  (cond
	    ((null (cdr examples))
	     (gtk:box-pack-start
	      box (exemplary-full-example (car examples))))
	    (t
	     (dolist 
	    

(defun exemplary (doc-example)
  (with-slots (args arg-examples) doc-example
    (tabs-like
     (mapcan
      (lambda (a)
	(case a
	  ((&key &optional &rest))
	  (t
	   (list(list
		 (make-instance 'gtk:toggle-button
		   :label (string-downcase (symbol-name (delist a))))
		 (exemplary-examples a (getf arg-examples a)))))))
      args))))

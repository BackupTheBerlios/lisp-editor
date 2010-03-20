;;Author: Jasper den Ouden
;;This file is in public domain.

(cl:in-package :cl-user)

(defpackage :generic
  (:nicknames :gen)
  (:use :common-lisp)
  (:export sqr delist intern* to-keyword

	   constant curry curry-l compose-rest
	   
           with-gensyms for-more setf-
	   if-let if-use when-let case-let
	   or-list
	   ift when-do
	   string-case
	   clamp

	   mk with-mod-slots
	   with-access with-mod-access
	   
	   with-stream
	   
	   setf-defun
	   
	   var-changer defvar*)
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

(defun to-keyword (name)
  (if (keywordp name) name
    (intern (typecase name
	      (string name)
	      (symbol (symbol-name name))
	      (t (error "How to convert ~a to keyword?" name)))
	    :keyword)))

;(defun subseq* (..

(defmacro with-gensyms ((&rest vars)&body body)
"Makes you some variables with gensyms output in them."
  `(let (,@(mapcar (lambda (v) `(,v (gensym))) vars))
     ,@body))

(defmacro for-more (macroname &rest args)
  "Applies a series of different arguments to same function."
  (cons 'progn
     (loop for el in args
	collect (cons macroname el))))

(defmacro setf- (operator set &rest args)
  "Changes 'set argument with setf using given operator, and extra\
 arguments. WARNING/TODO: abstraction leak if set has sideeffects."
  `(setf ,set (,operator ,set ,@args)))

(defmacro if-let (var cond if-t &optional (if-f nil))
  "Makes a variable var set by cond, and them does if-t if non-nil and
 (optionally)if-f else."
  `(let ((,var ,cond))
    (if ,var ,if-t ,if-f)))

(defmacro if-use (&rest conds)
  "Essentially, just 'or' but assuming that the output is actually used.
Have become increasingly comfortable with just using or in this case."
  `(or ,@conds))

(defmacro when-let (var cond &body body)
  "When, but with the condition, var available."
  `(if-let ,var ,cond (progn ,@body) nil))

(defmacro case-let (var is &rest cases)
  "Case, but makes a variable for you."
  `(let ((,var ,is))
     (case ,var ,@cases)))

(defmacro or-list (&rest orred)
  "If results anything, puts it in a list. Handy for ,@(or-list ...)"
  (with-gensyms (got)
    `(when-let ,got (or ,@orred)
       (list ,got))))

(defmacro ift (manner self &rest with)
  "Returns itself if `(,manner ,self ,@with) true."
  (with-gensyms (s)
    `(let ((,s ,self))
      (when (,manner ,s ,@with)
	,s))))

(defmacro when-do (cond &rest do)
  "return-from the condition, if the condition is true."
  (with-gensyms (s)
    `(when-let ,s ,cond
       (,@do ,s))))

(defmacro string-case (string &rest cases)
  "A case for strings."
  (cons 'cond
    (loop for el in cases
      collect (if (eql (car el) t)
		`(t ,(cadr el))
		`((string= ,string ,(car el)) ,(cadr el))))))

(defmacro let-from-list ((&rest vars) list)
  "Sets given variables vars according to list, as far as possible."
  (with-gensyms (tmp)
    `(let ((,tmp ,list))
     (let 
       ,(loop for var in vars 
	      for i from 0
	  collect `(,var (nth ,i ,tmp)))))))

(defun clamp (clamped from to)
  "Clamp between two values."
  (cond ((< clamped from) from)
	((> clamped to)   to)
	(t                clamped)))

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

(defun constant (value)
  "Produces function with constant value."
  (lambda (&rest rest)
    (declare (ignore rest))
    value))

(defun curry (fun &rest curried)
  "Curry to the right; add arguments to the end of the function.
Note: uses apply.. Hope it will optimize."
  (lambda (&rest args)
    (apply fun (append args curried))))

(defun curry-l (fun &rest curried)
  "Curry to the left; add arguments to the start of the function.
Note: uses apply.. Hope it will optimize."
  (lambda (&rest args)
    (apply fun (append curried args))))

(defun compose-rest (fun to)
  "Composes two functions."
  (lambda (&rest args)
    (funcall fun (apply to args))))

(defmacro setf-defun (name (&rest args) &body body)
  "Make a defun and a setter at the same time.
TODO see though things."
  (with-gensyms (to)
    `(progn (defun ,name (,@args) ,@body)
	    (defun (setf ,name) (,to ,@args)
	      ,(car body)
	      (setf ,@(last body) ,to)))))

(defmacro var-changer (var-name
	           &key (doc "Changes a variable, see the variable doc."))
  "Makes a variable changer for a given variable."
  `(defmacro ,var-name (to &body body)
     ,doc
     (append (list 'let (list (list ',var-name to)))
	     body)))

(defmacro defvar* (var-name &optional init doc (changer-doc doc))
  "Makes variable, then makes it changable"
  `(progn (defvar ,var-name ,init ,doc)
	  (var-changer ,var-name :doc ,changer-doc)))

(defmacro with-stream ((stream value &key start end
			           (direction :input) element-type
			           if-exists if-does-not-exist
				   external-format) &body body)
  "Makes a stream based on the value, the keyword arguments correspond to\
 the functions. Distinguishing between:
  *streams used as-is, not closed of course.
  *string via make-string-input-stream or make-string-output-stream\
 depending on direction.
  *files(pathnames) (via open, of course)
What is returned is usually the last input, unless it is string-output."
  (with-gensyms (val last dir)
    (macrolet ((k (sym)
		 "Keyword-entering convenience."
		 `(when ,sym
		    (list ,(intern
			    (symbol-name sym) (find-package :keyword))
			  ,sym))))
      `(let*((,val ,value)
	     (,dir ,direction)
	     (,stream
	      (cond
		((pathnamep ,val)
		 (open ,val ,@(k direction) ,@(k element-type)
		       ,@(k if-exists) ,@(k if-does-not-exist)
		       ,@(k external-format)))
		((stringp ,val)
		 (case ,dir
		   (:input
		    (make-string-input-stream ,val ,@(k start) ,@(k end)))
		   (:output
		    (make-string-output-stream ,@(k element-type)))))
		(t
		 ,val))))
	 ,@(butlast body)
	 (let ((,last ,(car(last body))))
	   (cond ((pathnamep ,val)
		  (close ,stream)
		  ,last)
		 ((stringp ,val)
		  (case ,dir
		    (:input  ,last)
		    (:output (get-output-stream-string ,stream))))
		 (t
		  ,last)))))))

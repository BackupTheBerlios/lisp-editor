;;Author: Jasper den Ouden
;;This file is in public domain.

(require :iterate)

(defpackage #:generic
  (:nicknames #:gen)
  (:use #:common-lisp #:iterate)
  (:export sqr
	   delist
           with-gensyms for-more setf-
	   if-let if-use when-let case-let
	   cond* ift when-do
	   string-case
	   clamp

	   with-mod-slots)
  (:documentation "Assortment of little useful macros/functions."))

(in-package #:generic)

(defun sqr(x)
  "Square of a value."
  (* x x))

(defun delist (x)
  "If list, return car, otherwise itself."
  (if (listp x) (car x) x))

(defmacro with-gensyms ((&rest vars)&body body)
"Makes you some variables with gensyms output in them."
  `(let (,@(iter (for el in vars)
		 (collect `(,el (gensym)))))
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
  "Returns the first one that returns non-nil."
  (if (null(cdr conds))
    (car conds)
    (with-gensyms (var)
      `(if-let ,var ,(car conds) ,var
	 (if-use ,@(cdr conds))))))

(defmacro when-let (var cond &body body)
  "When, but with the condition, var available."
  `(if-let ,var ,cond (progn ,@body) nil))

(defmacro case-let (var is &rest cases)
  "Case, but makes a variable for you."
  `(let ((,var ,is))
     (case ,var ,@cases)))

(defmacro cond* (&rest clauses)
  "Cond where keywords at start of clauses mean things.
:let makes a single conditition a variable for you, available in body.
:with(*) makes a bunch of variables which is then available for the clause\
 condition and body.
:or-let(*) and :and-let make you a series of variables that all have to be\
 true/false."
  (flet ((make-multi-let-type (c i let comb)
	   `(t (,let (,@(cadr c))
		 (if (,comb ,@(iter (for el in (cadr c))
				    (collect (car el))))
		   (progn ,@(cddr c))
		   (cond* ,@(cdr i))))))
	 (make-with (c i let)
	   `(t (,let (,@(cadr c))
		 (if ,(caddr c)
		   (progn ,@(cdddr c))
		   (cond* ,@(cdr i)))))))
  `(cond
     ,@(iter (for i on clauses)
	     (for c in clauses)
	     (case (car c)
	       (:with    (collect (make-with c i 'let))
			 (finish))
	       (:with*   (collect (make-with c i 'let*))
			 (finish))
	       (:let
		(collect `(t (if-let ,(cadr c) ,(caddr c)
				(progn ,@(cdddr c))
				(cond* ,@(cdr i)))))
		(finish))			     
	       (:or-let  (collect (make-multi-let-type c i 'let 'or)))
	       (:or-let* (collect (make-multi-let-type c i 'let* 'or)))
	       (:and-let (collect (make-multi-let-type c i 'let 'and)))
	       (:and-let*(collect (make-multi-let-type c i 'let* 'and)))
	       (t        (collect c)))))))

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

(defmacro with-mod-slots (mod (&rest slots) object &body body)
  "WITH-SLOTS, but requires something to be prepended to the\
 SYMBOL-MACROLET's, this allows you to use two or more objects with\
 convenient symbols at the same time."
  (with-gensyms (obj)
    `(let ((,obj ,object))
       (symbol-macrolet
	   (,@(iter
	       (for slot in slots)
	       (collect `(,(intern (format nil "~D~D" mod slot))
			   (slot-value ,obj ',slot)))))
	 ,@body))))

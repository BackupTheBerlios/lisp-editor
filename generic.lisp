;;Author: Jasper den Ouden
;;This file is in public domain.

(defpackage #:generic
  (:nicknames #:gen)
  (:use #:common-lisp #:iterate)
  (:export sqr
<<<<<<< .merge_file_EnfrYO
	   delist
=======
	   swap delist
>>>>>>> .merge_file_KRd2BM
           with-gensyms for-more setf-
	   if-let if-use when-let case-let
	   cond* ift when-do
	   string-case
<<<<<<< .merge_file_EnfrYO
	   clamp)
  (:documentation "Assortment of little useful macros/functions."))
=======
	   in-list clamp
	   before-out
	   and* or*))
>>>>>>> .merge_file_KRd2BM

(in-package #:generic)

<<<<<<< .merge_file_EnfrYO
(defun sqr(x)
  "Square of a value."
  (* x x))
=======
(defun sqr(x) (* x x))
>>>>>>> .merge_file_KRd2BM

<<<<<<< .merge_file_EnfrYO
(defun delist (x)
  "If list, return car, otherwise itself."
  (if (listp x) (car x) x))
=======
(defun delist (x) (if (listp x) (car x) x))

(defmacro swap (a b &optional tmp)
"Swaps two variables." ;TODO abstraction leak from state changing setf functions.
  (let*((tmp2 (if tmp tmp (gensym)))
        (out `((setf ,tmp2 ,a)
	       (setf ,a ,b) (setf ,b ,tmp2))))
    (if tmp
      `(progn ,@out)
      `(let (,tmp2) ,@out))))
>>>>>>> .merge_file_KRd2BM

(defmacro with-gensyms ((&rest vars)&body body)
"Makes you some variables with gensyms output in them."
  `(let (,@(iter (for el in vars)
		 (collect `(,el (gensym)))))
     ,@body))

<<<<<<< .merge_file_EnfrYO
=======
(defmacro before-out ((&body out) &body before)
"Allows you to have a body after something you want to return without temporary 
variable. (It is in the macro output, of course.)"
(with-gensyms (ret)
  `(progn
     (let ((,ret (progn ,@out)))
       ,@before
       ,ret))))

>>>>>>> .merge_file_KRd2BM
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
<<<<<<< .merge_file_EnfrYO
=======
	    

(defun in-list (list what)
  "Returns whether what is in list."
  (do ((i list (cdr i)))
      ((or (null i) (eql (car i) what))  (car i))))
>>>>>>> .merge_file_KRd2BM

(defun clamp (clamped from to)
  "Clamp between two values."
  (cond ((< clamped from) from)
	((> clamped to)   to)
	(t                clamped)))
<<<<<<< .merge_file_EnfrYO
=======

(defmacro and* (&rest args)
  "And, but executed in sequence. (You can rely on previous entries being\
 true.)"
  (if (null(cdr args))
     (car args)
    `(if ,(car args)
	 (and* ,@(cdr args))
	 nil)))

(defmacro or* (&rest args)
  "Sequential or."
  (if (null(cdr args))
    (car args)
    `(if ,(car args)
	 t
	 (or* ,@(cdr args)))))
>>>>>>> .merge_file_KRd2BM

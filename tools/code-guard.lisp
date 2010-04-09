;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl)

(defpackage :code-guard
  (:use :common-lisp :alexandria :expression-hook)
  (:export trust-p thrust-these trust-expr-p)
  (:documentation "Tells you whether to trust expressions, allowing only\
 certain symbols and packages.
Note that there should be a tight lease on everything.

TODO Work in progress (And may turn out too hard to do this way.)"))

(in-package :code-guard)

(defvar *trust* (make-hash-table :test 'equalp)
  "Trusted symbols/packages, WARNING: nil is maybe, :no is definitely not,\
 :yes is definitely yes, it is not t/nil")

(defun trust-p (in)
  "Whether something is trusted."
  (the (or null (eql :yes) (eql :no))
    (typecase in
    ;These and before refers to packages.
      (string  (gethash in *trust*))
      (package (trust-p (package-name in)))
    ;This one to symbols in general.
      (symbol  (or (gethash in *trust*)
		   (trust-p (symbol-package in))))
    ;Function/variable like trust.
      (list    (or (gethash in *trust*)
		   (gethash (cadr in) *trust*)))
      (t       (error "I don't do this: ~s" in)))))

(defun (setf trust-p) (to in)
  "Set whether something is trusted."
  (declare (type (or null (eql :yes) (eql :no))))
  (typecase in
    (keyword (setf (trust-p (find-package in)) to))
    (string  (setf (gethash in *trust*) to))
    (package (setf (trust-p (package-name in)) to))
    (symbol  (setf (gethash in *trust*) to))
    (list    (setf (gethash in *trust*) to))
    (t       (error "I don't do this: ~s" in))))

(defun trust-list (way list)
  (dolist (el list) (setf (trust-p el) way)))

(trust-list :yes ;Trust these from CL:
 '(defun defvar defparameter
   
   cond if when unless
   function funcall lambda
   let let* flet labels destructuring-bind multiple-value-bind
   
   + - * / > < >= =< 
   mod truncate floor ceiling
   abs cos sin tan acos asin atan exp expt log

   complex complexp number numberp integer integerp

   setq setf

   values

   ;These _must_ be guarded and considered potential threats.
   open intern
   ))
 
(defun trust-expr-p (expr)
  "Whether and expression is trusted. Disregards distinction between\
 functions/variables."
  (typecase expr
    ((or null keyword) ;TODO can't go round trusting keyword, 
     :yes)
    (symbol
     (trust-p expr))
    (list
     (if-let (distrust
	      (find-if-not (lambda (e) (eql :yes (trust-expr-p e))) expr))
       (values :no distrust) :yes))
    ((or string number complex character)
     :yes)
    (t
     (error "Didn't recognize ~s and should be suspicious." expr))))

(defun trust-expand-hook
    (expr &key (continue-expand
		(if-let (pkg (find-package :expression-hook))
		  (intern "EXPAND-HOOK" pkg)
		  #'identity)))
  "Expands expression, stopping if it sees anything it doesn't trust."
  (declare (type (or symbol function continue-expand)))
  (labels
      ((distrust ()
	 (return-from trust-expand-hook (values 'distrusted! :no)))
       (must-trust (thing)
	 (unless (eql (trust-p thing) :yes) (distrust))))
    (let ((*expression-hook*
	   (lambda (expr)
	     (typecase expr
	       (null)
	       (symbol
		(must-trust (list 'variable expr))
		expr)
	       (list
		(must-trust (list 'function (car expr)))
		(case (car expr)
		  (quote
		   (unless (eql :yes (trust-expr-p (cadr expr)))
		     (distrust)))
		  (function
		   (when (symbolp (cadr expr))
		     (must-trust (list 'function (cadr expr))))))
		(funcall continue-expand expr))))))
      (values (expand expr) :yes))))


;Trust everything from the keyword package.
(setf (trust-p (find-package :keyword)) :yes)

;;And guards.
(defvar *guards* nil "List of guards.")

(defun guard-name (name)
  (declare (type symbol name))
  (intern (concatenate 'string "GUARD-" (symbol-name name))))

(defmacro def-guard (name (&rest args) &body body)
  "Defines a guard onto a function. Essentially you redefine the \
function/macro see if anything malicious can be done with it, and\
 disallowing it if so. (It must also be clear what sort of things are\
 allowed.)"
  (with-gensyms (form)
    `(flet ((alarm (datum &rest arguments)
	      "Function to call if the guards finds something it\
 distrusts.)"
	      (apply #'error (cons datum arguments))
	      (values))
	    (guard-assert (test-form datum &rest arguments)
	      (when test-form
		(apply #'alarm (cons datum arguments)))))
       (pushnew ',name *guards*)
       (defun ,(guard-name name) (&rest ,form)
	 (destructuring-bind (,@args) ,form ,@body)))))

(defmacro guarded (() &body body)
  "Checks if the symbols are allowed, and executes body with guards"
  (dolist (expr body)
    (multiple-value-bind (trust-p distrust) (trust-expr-p expr)
      (unless (eql trust-p :yes)
	(error "Don't trust the body. Distrust ~s" distrust))))
  `(macrolet
       (,@(mapcan
	   (lambda (name)
	     (with-gensyms (args)
	       (when (macro-function name)
		 `((,name (&rest ,args)
		     (apply (function ,(guard-name name)) ,args))))))
	   *guards*))
     (flet (,@(mapcan
	       (lambda (name)
		 (unless (macro-function name)
		   `((,name (&rest args)
		      (apply (function ,(guard-name name)) args)))))
	       *guards*))
       ,@body)))
  
(def-guard intern (name &optional (package *package*))
  "Guarding symbols, important because functions call be funcalled thusly"
  (if (trust-p package)
    (intern name package)
    (let ((result (intern name package)))
      (if (trust-p result)
	result
	(alarm "Disallowed symbol ~s" result)))))

(def-guard apply (fun args)
  (typecase fun
    (symbol
     (assert (trust-p fun) nil "BUG: symbol found here should already have\
 been noticed as not being trusted!")
     (if (find fun *guards*) ;But this is really what it is about:
       (apply (guard-name fun) args) fun))
    (function
     (apply fun args))))

(def-guard funcall (fun &rest args)
  (guard-apply fun args))
#|(guard ()

;;Which filesystems are allowed for reading/writing?
; All relative to current.
 (defvar *allowed-state* nil)


 (def-guard open (filename
		 &key (direction :input) (element-type 'base-char)
		 if-exists if-does-not-exist (external-format :default))
  (cond
    ((intersection '(:absolute :up)
		   (pathname-directory (pathname filename)))
     (error "You may not affect anything from the root. Got ~s" filename)
     (values))
    (

 (pathname-directory "jasper/proj")
|#

(sb-ext:unlock-package :cl)

(guarded ()
  (intern "Hax"))

(guarded ()
  (funcall (intern "INTERN") "+"))

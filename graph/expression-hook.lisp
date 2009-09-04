;;; Macroexpand dammit -- a portable code walker for Common Lisp
;;; Written by John Fremlin at MSI (http://www.msi.co.jp) Released
;;; into the public
;;; domain. http://john.freml.in/macroexpand-dammit
;;; Modified(much) by Jasper den Ouden.

(cl:in-package :cl)

(defpackage #:expression-hook
  #+lispworks (:import-from #:lispworks #:compiler-let)
  (:use #:cl #:generic)
  (:export hook-expand expand-hook
	   *expression-hook* *expression-hook-state*)
  (:documentation 
   "Transforms code to return a quoted version its macroexpansion\
 using the host lisp to implicitly augment the lexical environment.\
 Expands macros, macrolets, symbol-macros, symbol-macrolets, and\
 compiler-macros.  Removes macrolets and symbol-macrolets."))

;;; Supports a few non-standard special forms for current (2009) Lisps.


(in-package #:expression-hook)

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defvar *form-handler* (make-hash-table))

  (defun force-first (x)
    "If list, first element."
    (if (listp x) (first x) x))
  (defun force-list (x)
    "If not list, put in list."
    (if (listp x) x (list x))))

(defvar *env* nil)

(defparameter *expression-hook* 'expand-hook
  "Hook that reads every s-expression macroexpand-dammit encounters.")

(defun binding-to-symbol (binding)
  (let ((name (force-first binding)))
    (cond ((listp name)
	   (assert (eq 'setf (first name)))
	   (check-type (second name) symbol)
	   (second name))
	  (t
	   name))))

(defmacro with-imposed-bindings (&body body)
  `(locally ,@body)
  #+sbcl
  (destructuring-bind ((binder bindings &rest binder-body))
      body
    `(locally
	 (declare (sb-ext:disable-package-locks ,@(mapcar 'binding-to-symbol bindings)))
	 (,binder ,bindings 
		  ,@binder-body))))

(defmacro without-package-locking (&body body)
  `(
    #. (progn 'progn
	      #+sbcl 'sb-ext:without-package-locks)
       ,@body))

(defmacro defhandler (symbol lambda-list &body body)
  "? Adds a handler to the symbol(s).\
 Handlers are basically macros prioritized by macroexpand-dammit?"
  (let ((syms (force-list symbol)))
    (let ((func (intern (format nil "~A~A" 'hander- (first syms)))))
      `(progn
	 (defun ,func ,lambda-list ;Define function named after first.
	   ,@body)
	 (setf
	  ,@(loop for sym in syms ;Set *form-handlers* accordingly.
		  collect `(gethash ',sym *form-handler*)
		  collect `',func))))))


(defhandler (progn locally) (progn &rest body)
  `(list ',progn
	 ,@(e-list body)))
	

(defhandler let (let bindings &rest body)
  (let ((names (loop for binding in bindings 
		     collect 
		     (force-first binding))))
    `(list*
      ',let
      (list 
       ,@(loop for binding in bindings
	       collect 
	       (if (symbolp binding)
		   `',binding
		   `(list ',(first binding)
			  ,@(e-list (rest binding))))))
      (with-imposed-bindings
	(,let ,names
	  (declare (ignorable ,@names))
	  (m-list ,@body))))))


(defun dump-fbinding (name lambda-list &rest body)
  (let (bound-vars)
    (labels
	((binding-vars (&rest body)
	   `(let ,bound-vars
	      (declare (ignorable ,@bound-vars))
	      (m-list ,@body)))
	 (l (lambda-arg)
	   (cond ((member lambda-arg lambda-list-keywords)
		  `',lambda-arg)
		 (t
		  (destructuring-bind
			(var &optional (val nil val-present-p) present-var)
		      (force-list lambda-arg)
		    (prog1
			(if (listp lambda-arg)
			    `(list ',var ,@(when val-present-p `((car ,(binding-vars val)))) 
				   ,@(when present-var `(',present-var)))
			    `',var)
		      (push var bound-vars)
		      (when present-var (push present-var bound-vars))))))))
      `(list* ',name (list ,@(mapcar #'l lambda-list))
	     ,(apply #'binding-vars body)))))

(defun dump-fbindings (bindings)
  "Dump-fbinding for multiple."
  `(list ,@(mapcar (lambda (f) (apply 'dump-fbinding f)) bindings)))

(defun declare-fbindings-ignorable (bindings)
  `(declare (ignorable
	     ,@(mapcar (lambda (f)
			 `(function ,(force-first f))) bindings))))

(defun maybe-locally (forms)
  (flet ((starts-with-declare ()
	   (and (listp (first forms)) (eq (first (first forms)) 'declare))))
   (cond ((or (rest forms) (starts-with-declare))
	  (list* (if (starts-with-declare) 'locally 'progn) forms))
	 (t
	  (first forms)))))

(defun compiler-macroexpand-1 (form &optional *env*)
  (let ((cm 
	 (and (listp form) (function-name-p (first form)) 
	      (compiler-macro-function (first form) *env*))))
      (if cm
	  (funcall *macroexpand-hook* cm form *env*)
	  form)))

(defun hook-expand (form)
  "Only applies expression hook.
If you want full expansion it should call expand-hook, defaultly, it does\
 so."
  (e form))

(defun e (form)
  (funcall *expression-hook* form))

(defmacro m (form &environment *env*)
  (e form))

(defmacro m-list (&body body &environment *env*)
  `(list ,@(e-list body)))

(defun e-list (list)
  "Expands a list of things to expand."
  (mapcar 'e list))

(defun expand-hook (form)
  "See macroexpand-dammit-expansion."
  (if-let handler-fn (and (listp form)
			  (gethash (first form) *form-handler*))
    (apply handler-fn form)
    (multiple-value-bind (form expanded) (macroexpand-1 form *env*)
      (cond
	(expanded
	 (e form))
	(t
	 (typecase form
	   (null nil)
	   (list
	     (let ((next (compiler-macroexpand-1 form))) ;?Why would it do something?
	       (if (eq form next)
		 (apply ;Only place where default happens.
		  (gethash (first form) 
			   *form-handler* #'default-form-handler)
		  form)
		 (e next))))
	   (t
	    `',form)))))))

(defun default-form-handler (first &rest rest)
  "Default handler of the form. "
  `(list ,(if (symbolp first)
	    `',first
	    (e first)) ,@(e-list rest)))

(defhandler declare (declare &rest body)
  `(list ',declare 
	 ,@(mapcar (lambda (f) `',f) body)))

(defhandler block (block name &rest body)
  `(list ',block ',name
	  ,@(e-list body)))

(defhandler return-from (return-from name &optional (value nil value-p))
  `(list ',return-from ',name
	 ,@(when value-p
		 `(,(e value)))))

(defhandler catch (catch tag &rest body)
  `(list ',catch ,(e tag) ,@(e-list body)))

(defhandler load-time-value
    (load-time-value form &optional (read-only-p nil rop-p))
  `(list ',load-time-value ,(e form) 
	,@(when rop-p
	    `(',read-only-p))))

(defhandler 
    (macrolet 
	symbol-macrolet 
      compiler-let ; mostly for Lispworks
  ) 
    (macrolet bindings &rest body)
  `(maybe-locally
    (with-imposed-bindings
      (,macrolet ,bindings
;; this does not work for macrolet and causes warnings
;,(declare-fbindings-ignorable bindings)
	(m-list ,@body)))))

(defhandler flet (flet bindings &rest body)
  `(list* ',flet
	 ,(dump-fbindings bindings)
	 (with-imposed-bindings
	   (,flet ,bindings
	     ,(declare-fbindings-ignorable bindings)
	     (m-list ,@body)))))

(defhandler labels (labels bindings &rest body)
  `(with-imposed-bindings
     (,labels ,bindings
       ,(declare-fbindings-ignorable bindings)
       (list* ',labels
	      ,(dump-fbindings bindings)
	      (m-list ,@body)))))

(defhandler let* (let* bindings &rest body)
  (if (not bindings)
      (e `(locally ,@body))
      (destructuring-bind (first &rest rest)
	  bindings
	(e `(let (,first)
	      ,@(if rest
		   `((,let* ,rest ,@body))
		   body))))))

(defhandler eval-when (eval-when situation &rest body)
  `(list ',eval-when ',situation
	 ,@(e-list body)))

#+sbcl
(defhandler sb-int:named-lambda (named-lambda name lambda-list &rest body)
  `(list* ',named-lambda ,(apply 'dump-fbinding name lambda-list body)))

(defhandler defun (defun name lambda-list &rest body)
  `(list* ',defun ,(apply 'dump-fbinding name lambda-list body)))

(defhandler lambda (lambda lambda-list &rest body)
  (apply 'dump-fbinding lambda lambda-list body))

(defun tagbody-restore-tags (list)
  (loop for f in list
	collect 		    
	(cond ((or (symbolp f) (integerp f))
	       `(progn ,f))
	      ((and (listp f) (eq 'tagbody-restore-tag (first f)))
	       (second f))
	      (t
	       f))))

(defhandler tagbody (tagbody &rest tags-and-forms)
  `(list* ',tagbody
	  (tagbody-restore-tags 
	   (list
	    ,@(loop for f in tags-and-forms 
		    collect
		    (if (or (symbolp f) (integerp f))
			`(list 'tagbody-restore-tag ',f)
			(e f)))))))

(defhandler setq (setq &rest pairs)
  (declare (ignore setq))
  (let ((vars (loop for s in pairs by #'cddr collect (macroexpand s *env*))))
    (let ((expanded (loop for n in vars for r in (rest pairs) by #'cddr
			  collect n collect r)))
      (if (some 'listp vars)
	  (e `(setf ,@expanded))
	  `(list 'setq ,@(e-list expanded))))))

(defun function-name-p (name)
  (or (symbolp name) 
      (and (listp name)
	   (eq (first name) 'setf) (symbolp (second name))
	   (not (cddr name)))))

(defhandler function (function name)
  `(list ',function
	 ,(if (function-name-p name)
	      `',name
	      (e name))))

(defhandler the (the value-type form)
  `(list ',the ',value-type ,(e form)))

(defhandler go (go tag)
  `(list ',go ',tag))

(defhandler unwind-protect (unwind-protect protected-form &rest cleanup)
  `(list ',unwind-protect ,(e protected-form) ,@(e-list cleanup)))

(defhandler progv (progv symbols values &rest body)
  `(list ',progv
	 (list ,@(e-list symbols))
	 (list ,@(e-list values))
	 ,@(e-list body)))

(defhandler quote (quote object)
  `(list ',quote ',object))

#|
;;; Some shenanigans to support running with or without swank
 (defun runtime-symbol (name package-name)
  (or (find-symbol (symbol-name name)
		   (or (find-package package-name) (error "No package ~A" package-name)))
      (error "No symbol ~A in package ~A" name package-name)))
 (defun macroexpand-dammit-string (str)
  (funcall (runtime-symbol 'apply-macro-expander 'swank) 'macroexpand-dammit str))
|#
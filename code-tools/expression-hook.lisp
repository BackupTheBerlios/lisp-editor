;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :expression-hook
  (:use :common-lisp :alexandria :denest :package-stuff)
  (:nicknames :expr-hook)
  (:export expand expand-hook expand-mac *expression-hook*
	   *in-funs* *eh-funs* *eh-vars* *eh-macs* *eh-sym-macs*
	   expression-hook-to-macro-hook macro-hook-to-expression-hook)
  (:documentation 
   "Macroexpands code purely by itself. *expression-hook* continues it,
 and must call further expand-hook.
Used for gathering information on code autodoc via expression-scan."))

(in-package :expression-hook)

;;TODO Got a bug about macrolet, seen first through defmethod

(defparameter *expression-hook* 'expand-hook
  "Hook that reads every s-expression macroexpand-dammit encounters.")

(defun expression-hook-to-macro-hook (expr-hook)
  "(Trivial)conversion from expression hook to macro hook."
  (lambda (expander form env)
    (funcall expr-hook form)
    (funcall expander form env)))

(defun macro-hook-to-expression-hook (macro-hook)
  "(Trivial)conversion from macro hook to expression hook."
  (lambda (expr)
    (funcall macro-hook #'funcall expr nil)))

(defvar *base-macros* (make-hash-table)
  "Base macros, but can also override regular macros.(That case you have to\
 call expand again to continue.)
 TODO possible warnings if you do")

(defvar *in-funs* (list)
  "Current functions. (Can be in multiple; like a defun and then a flet.")

(defvar *eh-funs* (list)
  "List of functions that are from local flet, labels.")
(defvar *eh-macs* (list)
  "List of functions that are from local macrolet.")

(defvar *eh-vars* (list)
  "List of variables that are from local let, let*.")

(defvar *eh-sym-macs* (list)
  "List of symbol-macros.")

(defvar *discontinue* nil)

(defmacro def-base-macro (name (&rest args) &body body)
  "Adds a base macro. These are macros that are not expanded by the\
 expander, but they do define how to scan on from these macros."
  (let ((form (gensym))
	(base-macro (intern (format nil "BASE-MACRO-~D"
				    (if (listp name) (car name) name)))))
    `(flet ((,base-macro (,form)
	      (destructuring-bind (,@args) ,form
		,@body)))
       ,@(cond
	  ((listp name)
	   (mapcar (lambda (n) `(setf (gethash ',n *base-macros*)
				      #',base-macro)) name))
	  (t	   
	   `((setf (gethash ',name *base-macros*) #',base-macro)))))))

(defparameter *ignore-packages* ;TODO SBCL specific.
  (list :sb-impl :sb-int :sb-c :sb-pcl :sb-kernel :sb-loop :sb-vm
	:sb-di)
  "Packages, when encountered, it does not scan the contents.")

(defun expand (form)
  "Macroexpands form according to the macros and the 'base macros'\
 (which are intended to 'replace' special operators.)"
  (funcall *expression-hook* form))

(defun expand-hook (form &key as-macrohook)
  "The hook version of expand; expand works, only expanding, with this\
 expression hook. You have to call this with your expression hook and use\
 lexical binds to pass information down."
  (denest ;Denest, because COND won't let you make vars.
   (if *discontinue* nil)
   (typecase form
     (null
      form)
    ;Apply possible symbol-macros.
     (symbol
      (if-let (sm (assoc form *eh-sym-macs*))
	(caddr sm) form))
  ;Something need not care about.
     ((not list) ;Oh typecase is so awesome.
      form)
  ;Functions not starting with a symbol. TODO got them all?
     ((cons list)
      (case (caar form)
	(lambda
	   (with-gensyms (fn)
	     (expand-hook `(flet ((,fn ,@(cdar form)))
			     (,fn ,@(cdr form))))))
	(t (warn "Unrecognized s-expression car-list. ~a" (car form))
	   form))))
   (t) ;And other cases:
  ;Ignored parts.
   (if (find (symbol-package (car form)) *ignore-packages*
	     :test #'same-package)
       form)
  ;Possible macrolets.
   (if-let (m (assoc (car form) *eh-macs*))
     (expand (apply(cadr m) (cdr form))))
  ;Possible flets.
   (if (find (car form) *eh-funs*)
     `(,(car form) ,@(e-list (cdr form))))
  ;Base macros override existing macros.
   (if-let (bfn (gethash (car form) *base-macros*))
     (funcall bfn form))
  ;Regular macros.
   (if-let (mfn (macro-function (car form)))
     (if as-macrohook ;Macrohooks are picked up later.
       form
       (expand (funcall mfn form nil))))
  ;Just a regular function.
   `(,(car form) ,@(e-list (cdr form)))))

(defun e-list (body)
  "Short for (mapcar #'expand body)"
  (mapcar #'expand body))

(def-base-macro let (let (&rest vars) &body body)
  (denest
   (block base-let)
   (collecting (:onto binds :collect col-bind))
   (collecting (:onto *eh-vars* :collect col-var))
   (dolist (v vars `(,let (,@binds) ,@(e-list body)))
     (cond
       ((listp v) v
	(destructuring-bind (var &optional val) v
	  (col-bind `(,var ,(expand val)))
	  (col-var var)))
       (t
	(col-bind v) (col-var v))))))

(def-base-macro let* (let* (&rest vars) &body body)
  (assert (eql let* 'let*) nil "")
  (cond ((null vars)
	 (expand `(progn ,@body)))
	((null (cdr vars))
	 (expand `(let (,(car vars)) ,@body)))
	(t
	 (expand `(let (,(car vars)) (let (,@(cdr vars)) ,@body))))))

(def-base-macro symbol-macrolet (sm (&rest macs) &body body)
  (let ((*eh-sym-macs*
	 (append (mapcar (lambda (m)
			   (destructuring-bind (name to) m
			     (list name *in-funs* to)))
			 macs)
		 *eh-sym-macs*)))
    `(,sm (,@macs) ,@(e-list body))))

(def-base-macro load-time-value (load-time-value form &optional read-only-p)
  `(,load-time-value ,(expand form) ,(expand read-only-p)))

(defun base-fun (name args body &key flat-arg)
  "Base treatment of function."
  (declare (ignore flat-arg))
  (let ((*in-funs* (cons name *in-funs*)))
    `((,@(denest
	  (block args-block)
	  (let (state))
	  (collecting (:onto arg :collect col-arg))
	  (collecting (:onto key :collect col-key))
	  (collecting (:onto opt :collect col-opt))
	  (collecting (:onto rest :collect col-rest))
	  (after (return-from args-block
		   `(,@arg ,@(when opt `(&key ,@opt))
			   ,@(when key `(&optional ,@key))
			   ,@(when rest `(&rest ,@rest)))))
	  (dolist (a args)
	    (case a
	      ((&key &optional &rest) (setf state a))
	      (t (case state
		   (&key
		    (col-key
		     (if (listp a)
		       `(,(car a) ,(expand (cadr a))) a)))
		   (&optional 
		    (col-opt
		     (if (listp a)
			 `(,(car a) ,(expand (cadr a))) a)))
		   (&rest 
		    (col-rest a))
		   (t (col-arg a))))))))
      ,@(e-list body))))

(def-base-macro defun (defun name (&rest args) &body body)
  `(,defun ,name ,@(base-fun name args body :flat-arg t)))

(def-base-macro defmethod
    (defmethod name way/args &optional args/dstr dstr/body &body body)
  (multiple-value-bind (args dstr body)
      (cond
	((listp way/args)
	 (values way/args args/dstr (cons dstr/body body)))
	((listp args/dstr)
	 (values args/dstr dstr/body body))
	(t                 (error "")))
    (declare (ignore dstr))
    `(,defmethod ,name ,@(when (symbolp way/args) (list way/args))
       ,@(base-fun name args body :flat-arg t))))

(def-base-macro lambda (lambda (&rest args) &body body)
  `(,lambda ,@(base-fun '|lambda| args body :flat-arg t)))

(def-base-macro defmacro (defmacro name (&rest args) &body body)
  `(,defmacro ,name ,@(base-fun name args body)))

(def-base-macro defvar (defvar name &optional init doc)
  `(,defvar ,name ,@(when init
		      (list (let ((*in-funs* (cons name *in-funs*)))
			      (expand init))))
     ,@(when doc (list doc))))

(def-base-macro defparameter (defvar name &optional init doc)
  `(,defvar ,name ,@(when init
		      (list (let ((*in-funs* (cons name *in-funs*)))
			      (expand init))))
     ,@(when doc (list doc))))

(defun expand-funs (funs &key flat-arg)
  "Expands flet/macrolet input and returns the names in the second value."
  (denest
   (collecting (:onto cr :collect col-result))
   (collecting (:onto cn :collect col-names))
   (after (return-from expand-funs (values cr cn)))
   (dolist (fun funs)
     (destructuring-bind (name (&rest args) &body body) fun
       (col-result `(,name ,@(base-fun name args body :flat-arg flat-arg)))
       (col-names name)))))

(def-base-macro labels (flet (&rest funs) &body body)
  (multiple-value-bind (res names) (expand-funs funs :flat-arg t)
    (let ((*eh-funs* (append *eh-funs* names)))
      `(,flet (,@res) ,@(e-list body)))))

(def-base-macro flet (flet (&rest funs) &body body)
  (multiple-value-bind (res names) (expand-funs funs :flat-arg t)
    `(,flet (,@res)
       ,@(let ((*eh-funs* (append *eh-funs* names))) ;Only on the body here.
	   (e-list body)))))

(def-base-macro macrolet (macrolet (&rest funs) &body body)
  "TODO *eh-funs* only good for labels.. 'flets shouldnt see eachother'"
  (multiple-value-bind (res names) (expand-funs funs :flat-arg t)
    (declare (ignore res))
    (let ((*eh-macs*
	   (append
	    (mapcar (lambda (name)
		      (list name
			    (eval `(lambda ,@(cdr (assoc name funs))))))
		    names)
	    *eh-macs*)))
      `(,macrolet (,@funs) ,@(e-list body)))))

(defun function-name-p (name)
  (or (symbolp name) 
      (and (listp name)
	   (eq (first name) 'setf) (symbolp (second name))
	   (not (cddr name)))))

(def-base-macro function (function name)
  `(,function ,(if (function-name-p name)
		 name (expand name))))

(def-base-macro (progn locally) (progn &rest body)
  `(,progn ,@(e-list body)))

(def-base-macro progv (progv symbols values &rest body)
  "TODO has to register it's variables."
  `(,progv (,@(e-list symbols))
           (,@(e-list values))
     ,@(e-list body)))

(def-base-macro block (block name &body body)
  `(,block ,name ,@(e-list body)))

(def-base-macro return-from (return-from name &optional value)
  `(,return-from ,name ,(expand value)))

(def-base-macro catch (catch tag &body body)
  `(,catch ,(expand tag) ,@(e-list body)))

(def-base-macro tagbody (tagbody &body body)
  `(,tagbody ,@(e-list body)))

(def-base-macro go (go tag)
  `(,go ,tag))

(def-base-macro eval-when (eval-when situation &rest body)
  `(,eval-when ,situation
     ,@(e-list body)))

(def-base-macro setq (setq &rest pairs)
  `(,setq ,@(denest (summing (:onto k)) (collecting (:ret t))
		    (dolist (p pairs)
		      (collect (if (= (mod k 2) 0) p (expand k)))
		      (sum 1)))))

(def-base-macro unwind-protect (unwind-protect protected-form &rest cleanup)
  `(list ',unwind-protect ,(expand protected-form) ,@(e-list cleanup)))

(def-base-macro quote (quote object)
  `(,quote ',object))

(def-base-macro defpackage (&rest form)
  form)

(def-base-macro declare (declare &rest body)
  `(,declare ,@body)) ;Do nothing.

(def-base-macro the (the value-type form)
  `(,the ,value-type ,(expand form)))

;TODO eh disapearing error?
(def-base-macro psetq (&rest stuff) (declare (ignore stuff)) 
  nil)
(def-base-macro psetf (&rest stuff) (declare (ignore stuff)) 
  nil)

;;TODO will want to indicate we're in a defclass/defstruct??
(def-base-macro defclass
    (defclass name derive-from components &rest rest)
  (declare (ignore defclass name derive-from rest))
  (dolist (el components) ;Only expand the bits we want.
    (expand (getf (cdr el) :initform))))

(def-base-macro defstruct (defstruct name &rest slots)
  (declare (ignore defstruct name))
  (dolist (el slots) ;Only expand the bits we want.
    (when (listp el)
      (expand (cadr el)))))

(def-base-macro rotatef (&rest args)
  (e-list args))

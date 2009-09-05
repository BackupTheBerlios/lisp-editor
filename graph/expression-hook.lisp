
(cl:in-package :cl)

(defpackage :expression-hook
  (:use :cl :generic :denest :package-stuff)
  (:export expand expand-hook expand-mac *expression-hook*
	   *base-macros* def-base-macro
	   *eh-flet*)
  (:documentation 
   "Transforms code to return a quoted version its macroexpansion\
 using the host lisp to implicitly augment the lexical environment.\
 Expands macros, macrolets, symbol-macros, symbol-macrolets, and\
 compiler-macros.  Removes macrolets and symbol-macrolets.

Note that there are still annoyances; you have to be in the right package!\
 one with access to the scanned packages."))


(in-package #:expression-hook)

(defparameter *expression-hook* 'expand-hook
  "Hook that reads every s-expression macroexpand-dammit encounters.")

(defvar *base-macros* (make-hash-table)
  "Base macros, but can also override regular macros.(That case you have to\
 call expand again to continue.)
 TODO possible warnings if you do")

(defvar *eh-funs* (list)
  "List of functions that are from local flet.")

(defmacro def-base-macro (name (&rest args) &body body)
  "Adds a base macro by definition."
  (let ((form (gensym)))
    `(flet ((base-macro (,form)
	      (destructuring-bind (,@args) ,form
		,@body)))
       ,@(cond
	  ((listp name)
	   (mapcar (lambda (n) `(setf (gethash ',n *base-macros*)
				      #'base-macro)) name))
	  (t	   
	   `((setf (gethash ',name *base-macros*) #'base-macro)))))))

(defparameter *ignore-packages*
  (list :sb-impl :sb-int :sb-c :sb-pcl :sb-kernel) ;TODO SBCL specific.
  "Packages, when encountered, it stops scanning.")

(defun expand (form)
  "Macroexpands form according to the macros and the 'base macros'\
 (which are intended to 'replace' special operators.)"
  (funcall *expression-hook* form))

(defun expand-hook (form)
  "The hook version of expand; expand works, only expanding, with this\
 expression hook. You have to call this with your expression hook and use\
 lexical binds to pass information down."
  (when (not (listp form)) ;TODO add symbol-macros.
    (return-from expand-hook form))
  (let ((bfn (gethash (car form) *base-macros*)) ;Base macros.
	(mfn (macro-function (car form)))) ;Regular macros.
    (cond
      ((null form)
       form)
      ((find (symbol-package (car form)) *ignore-packages*
	     :test #'same-package)
       form)
      (bfn
       (funcall bfn form))
      (mfn
       (expand (funcall mfn form nil)))
      (t ;Apparently nothing can be done further.
       `(,(car form) ,@(e-list (cdr form)))))))

(defun e-list (body)
  (mapcar #'expand body))

(def-base-macro (progn locally) (progn &rest body)
  `(,progn
     ,@(e-list body)))
	
(def-base-macro (let let*) (let (&rest vars) &body body)
  "TODO if variables also tracked, we'll want a *eh-vars*
 (and split let and let*"
  `(,let (,@(mapcar (lambda (v)
		     (if (symbolp v) v
		       `(,(car v) ,(expand (cadr v))))) vars))
     ,@(e-list body)))

(def-base-macro declare (declare &rest body)
  `(,declare ,@body)) ;Do nothing.

(def-base-macro block (block name &body body)
  `(,block ,name
     ,@(e-list body)))

(def-base-macro return-from (return-from name &optional value)
  `(,return-from ,name ,(expand value)))

(def-base-macro catch (catch tag &body body)
  `(,catch ,(expand tag) ,@(e-list body)))

(def-base-macro load-time-value (load-time-value form &optional read-only-p)
  `(,load-time-value ,(expand form) ,(expand read-only-p)))

;;TODO persistent little warnings.. fix return-accumulate..
(defun expand-funs (funs)
  "Expands flet/macrolet input and returns the names in the second value."
  (denest (return-accumulate* ((collecting nil cr col-result)
			       (collecting nil cn col-names)))
	  (dolist (fun funs)
	    (destructuring-bind (name (&rest args) &body body) fun
	      (col-result `(,name (,@args) ,@(e-list args) ,@body))
	      (col-names name)))))

(def-base-macro (flet macrolet) (flet (&rest funs) &body body)
  "Does flet and macrolet, also tracks which names are taken by them in\
 *eh-funs*."
  (multiple-value-bind (res names) (expand-funs funs)
    (let ((*eh-funs* (append *eh-funs* names)))
      `(,flet (,@res) ,@(e-list body)))))

(def-base-macro labels (labels (&rest funs) &body body)
  (declare (ignore labels))
  (expand `(denest ,@(mapcar (lambda (fun) `(flet (,fun))) funs)
		   (progn ,@body))))

(def-base-macro symbol-macrolet (sm (&rest macs) &body body)
  "TODO feh, how does CL do this exactly..
 (Can i just flat-out-replace them?)"
  `(,sm (,@macs)
     ,@(e-list body)))

(def-base-macro eval-when (eval-when situation &rest body)
  `(,eval-when ,situation
     ,@(e-list body)))

(def-base-macro tagbody (tagbody &body body)
  `(,tagbody ,@(e-list body)))

(def-base-macro setq (setq &rest pairs)
  `(,setq ,@(denest (summing (0 k))
		    (collecting ())
		    (dolist (p pairs)
		      (collecting (if (= (mod k 2) 0) p (expand k)))
		      (summing 1)))))

(defun function-name-p (name)
  (or (symbolp name) 
      (and (listp name)
	   (eq (first name) 'setf) (symbolp (second name))
	   (not (cddr name)))))

(def-base-macro function (function name)
  `(,function ,(if (function-name-p name)
		 `',name (expand name))))

(def-base-macro the (the value-type form)
  `(,the ,value-type ,(expand form)))

(def-base-macro go (go tag)
  `(,go ,tag))

(def-base-macro unwind-protect (unwind-protect protected-form &rest cleanup)
  `(list ',unwind-protect ,(expand protected-form) ,@(e-list cleanup)))

(def-base-macro progv (progv symbols values &rest body)
  `(,progv
       (,@(e-list symbols))
       (,@(e-list values))
     ,@(e-list body)))

(def-base-macro quote (quote object)
  `(,quote ',object))

(def-base-macro defpackage (&rest form)
  form)

(def-base-macro defun (defun name (&rest args) &body body)
  `(,defun ,name (,@(denest* 
		    (let state)
		    (collecting nil arg col-arg)
		    (collecting nil key col-key)
		    (collecting nil opt col-opt)
		    (collecting nil rest col-rest)
		    (:* :return `(,@arg ,@(when opt `(&key ,@opt))
				     ,@(when key `(&optional ,@key))
				     ,@(when rest `(&rest ,@rest))))
		    (:* dolist (a args)
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
     ,@(e-list body)))

(def-base-macro defmacro (defmacro name (&rest args) &body body)
  "TODO read &key and &optional arguments all through.\
 (argumentize list has the function..)"
  `(,defmacro ,name (,@args) ,@(e-list body)))

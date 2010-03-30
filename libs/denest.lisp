;;Author: Jasper den Ouden
;;This file is in public domain.

(cl:in-package :cl-user)

(defpackage :denest
  (:use :common-lisp :generic :alexandria)
  (:export denest	 ;Accumulators and reducers. Here because they need to expose
         ;flet/macrolet to user.
	   after
	   
	   accumulating summing collecting appending
	   besting maximizing minimizing
	   
	   do-1 do-parallel)
  (:documentation "Macro to denest, remove nestedness of macros\
/functions. Was somewhat of a relevation to me and suprising that this\
 function isn't being screamed from the rooftops. Turns out that macros\
 like iterate might just be trying to fight nestedness.(Mostly)

A few macros are given, some are on keywords and specific to denest, to \
 save namespace.  They do _exactly_ the same as regular macros however! You\
 can use them with the USE-DENEST macro. The macros supplied in this packageIf
with non-keyword symbols are exported.

TODO remove some nastyness that might happen if denest is nested with self..
Can it be done?"))

(in-package :denest)

(defmacro denest (&rest forms)
  "Un-nests stuff, reconstructs macros in order with the body at the end.
Best and easiest description is:
 (defmacro denest (&rest args)
   (if (null (cdr args))
     (car args)
     `(,@(car args)
       (denest ,@(cdr args)))))
All the actual version also does is provide a top block."
  (labels ((expand (forms)
	     (if (null (cdr forms))
	       (car forms)
	       `(,@(car forms) ,(expand (cdr forms))))))
    `(block denest-top ,(expand forms))))

;;--------------------------------------------------------------------------

(defmacro after (after &body before)
  `(progn ,@before ,after))

;;Macros applicable to denesting. (But also by themselves.)

;;Accumulators and reducers.
(defmacro accumulating
    ((initial onto operation &optional (by-name 'accumulating) without-let)
     &body body)
  "Alter a variable with an operation, each time accumulation is called,\
 return the result.
Multiple accumulations need to go by different names."
  (when (symbolp operation)
    (setf operation (list operation)))
  
  `(let (,@(unless without-let `((,onto ,initial))))
     ,@(unless without-let `((declare (ignorable ,onto))))
     (macrolet ((,by-name (&rest args)
		  (when (null args)
		    (error "Accumulation use does nothing."))
		  (list 'setf ',onto
			(append '(,@operation ,onto) args))))
       ,@body)
     ,onto))

(defmacro summing
    ((&key (initial 0) (onto (gensym)) (by-name 'summing) without-let)
     &body body)
  "Sum everything asked to, return result."
  `(accumulating (,initial ,onto + ,by-name ,without-let) ,@body))

(defmacro collecting ((&key (init '(list)) (onto (gensym))
                            (collect 'collecting) (append 'appending)
                            (last (gensym)) (append-1 (gensym))
			    (ret t))
		      &body body)
  "Collect everything asked to, return result. (Also, appending)
If you want to use two different collectings, you need to provide the\
 collect argument.(To avoid namespace collision, and to separate the two.)"
  `(let ((,onto ,init) ,last)
     (declare (ignorable ,onto))
     (labels ((,append-1 (collected)
		(if (null ,onto)
		  (setf ,onto collected
		        ,last (last ,onto))
		  (setf (cdr ,last) collected
			,last (last ,last))))
	      (,append (&rest appended)
		(dolist (a appended)
		  (,append-1 a)))
	      (,collect (&rest collected)
		(,append-1 collected)))
       ,@body)
     ,(when ret onto)))

(defmacro besting ((valuator &key intermediate initial (best (gensym))
			     (by-name 'besting) (changer (gensym)))
		   &body body)
  "Find best variant of something using function."
  `(let (,@intermediate)
     (flet ((,changer (a b)
	      (if (,valuator a b ,@(mapcar #'delist intermediate))
		a b)))
       (accumulating (,initial ,best ,changer ,by-name) ,@body))))

(defmacro maximizing ((&optional (initial 0) (max (gensym))) &body body)
  "Maximizes a number. WARNING, TODO can get rid of initial?"
  `(besting (> :initial ,initial :best ,max :by-name maximizing)
     ,@body))

(defmacro minimizing ((&optional (initial 0) (min (gensym))) &body body)
  "Minimized a number. WARNING, TODO can get rid of initial?"
  `(besting (< :initial ,initial :best ,min :by-name  minimizing) ,@body))

;;Iterating
(defvar *do-parallel-hash* (make-hash-table)
  "Hashtable with defined do-parallels.")

(defmacro def-do-parallel (name (&rest args) &key var until sym-mac)
  "Define an operation for do-parallel.
:return, :hash-table are overridden, and others are also defined on\
 keywords."
  `(setf (gethash ',name *do-parallel-hash*)
	 (lambda (,@args)
	   (values ,var ,until ,sym-mac))))

(def-do-parallel :do (var init change) :var `(,var ,init ,change))
(def-do-parallel :until (until) :until until)
(def-do-parallel :while (while) :until `(not ,while))
(def-do-parallel :range (var from to &optional (by 1))
  :var `(,var ,from (+ ,var ,by)) :until `(> ,var ,to))

(def-do-parallel :times (cnt &optional (var (gensym "do-parallel-times")))
  :var `(,var 0 (+ ,var 1)) :until `(>= ,var ,cnt))

(def-do-parallel :list (var &optional (list nil) (by 'cdr)
			   (iter-var (gensym "do-parallel-list")))
  :var `(,iter-var ,list (,by ,iter-var)) :until `(null ,var)
  :sym-mac `(,var '(car ,iter-var)))

(def-do-parallel :array (var array
			    &key (by 1) (from 0) (to `(length ,array))
			    (i (gensym "do-parallel-array")))
  :var `(,i ,from (+ ,i ,by)) :until `(>= ,var ,to)
  :sym-mac `(,var '(aref ,array ,i)))

(defmacro do-parallel ((&rest clauses) &body body)
  "Iterate various ways in parallel."
  (denest
   (let ((hash nil) (return '(values))))
   (collecting (:onto do-vars :collect col-var :ret nil))
   (collecting (:onto do-until :collect col-until :ret nil))
   (collecting (:onto sym-macs :collect col-sym :ret nil)
     (dolist (clause clauses)
       (case (car clause)
	 (:hash-table
 	   (assert (null hash) nil
		   "(TODO?)Can do only one hash in parallel.")
	   (setf hash (cdr clause)))
	 (:return
	   (setf return (cadr clause)))
	 (t
	  (multiple-value-bind (var until sym-mac)
	      (apply (gethash (car clause) *do-parallel-hash*)
		     (cdr clause))
	    (col-var var)
	    (col-until until)
	    (col-sym sym-mac)))))
     (return-from do-parallel 
       `(symbol-macrolet (,@sym-macs)
	  ,(if hash
	     (with-gensyms ((top-block "to-parallel-block"))
	       `(block ,top-block
		  `(let (,@(mapcar (rcurry #'subseq 0 2) do-vars))
		     (maphash (lambda (,(car hash))
				,@body
				(setf ,@(mapcan #'cdr do-vars))
				(when (or ,@do-until)
				  '(return-from top-block ,return)))
			      ,(cadr hash))
		     ,return)))
	     `(do (,@do-vars) ((or ,@do-until) ,return)
		,@body)))))))

(defmacro do-1 (clause &body body)
  "Iterates over one thing."
  `(do-parallel (,clause) ,@body)) ;do-parallel has it covered.

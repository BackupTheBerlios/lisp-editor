;;Author: Jasper den Ouden
;;This file is in public domain.

(cl:in-package :cl-user)

(defpackage :denest
  (:use :common-lisp)
  (:export denest def-denest-macro use-denest-macro
	 ;Accumulators and reducers. Here because they need to expose
         ;flet/macrolet to user.
	   accumulating summing collecting appending
	   besting maximizing minimizing
	   return-accumulate return-accumulate*)
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
All the actual version does is attach denest-specific macros on keywords,
and provide a top block."
  (labels ((expand (forms)
	     (cond
	       ((null (cdr forms))
		(car forms))
	       ((keywordp (caar forms))
		`(use-denest-macro ,@(car forms) (denest ,@(cdr forms))))
	       (t
		`(,@(car forms) ,(expand (cdr forms)))))))
    `(block denest-top ,(expand forms))))

(defvar *denest-macros* (make-hash-table)
  "Macros for denest when given a keyword.")

(defmacro def-denest-macro (name (&rest args) &body body)
  "Create a macro applied when denests meets an keyword.\
 :return is reserved."
  (unless (keywordp name)
    (error "Denest-specific macros need to be set on keyword; if the\
 namespace isn't taken, you could just replace def-denest-macro with\
 defmacro."))
  (let ((form (gensym)))
    `(flet ((denest-keyword-macro (,form)
	     (destructuring-bind (,@args) ,form
	       ,@body)))
       (setf (gethash ,name *denest-macros*) #'denest-keyword-macro))))

(defun synonym (of &rest syns)
  (let ((of (gethash of *denest-macros*)))
    (mapcar (lambda (name) (setf (gethash name *denest-macros*) of))
	    syns)))

(defmacro use-denest-macro (name &rest args)
  "Applies a denest macro."
  (funcall (gethash name *denest-macros*) args))

;;--------------------------------------------------------------------------

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

(defmacro summing ((&optional initial (onto (gensym)) without-let)
		   &body body)
  "Sum everything asked to, return result."
  (unless initial (setf initial 0)) ;This way, for sake of return-accumulate
  `(accumulating (,initial ,onto + summing ,without-let) ,@body))

(defmacro collecting ((&optional (initial '(list)) (onto (gensym))
				 (collect 'collecting) (append 'appending)
				 (last (gensym)) (append-1 (gensym)))
		      &body body)
  "Collect everything asked to, return result. (Also, appending)
If you want to use two different collectings, you need to provide the\
 collect argument.(To avoid namespace collision, and to separate the two.)"
  `(let ((,onto ,initial) ,last)
     (declare (ignorable ,onto))
     (flet ((,append-1 (collected)
	      (if (null ,onto)
		  (progn (setf ,onto collected)
			 (setf ,last (last ,onto)))
		  (setf (cdr ,last) collected
			,last (last ,last)))))
       (flet ((,append (&rest appended)
		(dolist (a appended)
		  (,append-1 a)))
	      (,collect (&rest collected)
		(,append-1 collected)))
	 ,@body))
     ,onto))

(defmacro besting ((valuator &key intermediate initial (best (gensym))
			     (by-name 'besting) (changer (gensym)))
		   &body body)
  "Find best variant of something using function."
  `(let (,@intermediate)
     (flet ((,changer (a b)
	      (if (,valuator a b ,@(mapcar (lambda (el)
					     (cond ((symbolp el) el)
						   ((listp el) (car el))))
					   intermediate))
		a b)))
       (accumulating (,initial ,best ,changer ,by-name) ,@body))))

(defmacro maximizing ((&optional (initial 0) (max (gensym))) &body body)
  "Maximizes a number. WARNING, TODO can get rid of initial?"
  `(besting (> :initial ,initial :best ,max :by-name maximizing)
     ,@body))

(defmacro minimizing ((&optional (initial 0) (min (gensym))) &body body)
  "Minimized a number. WARNING, TODO can get rid of initial?"
  `(besting (< :initial ,initial :best ,min :by-name  minimizing) ,@body))

;;Finishing.
(def-denest-macro :until ((cond &optional return (to 'denest-top)) 
			  &body body)
  `(if ,cond (return-from ,to ,return) (progn ,@body)))

(def-denest-macro :while ((cond &optional return (to 'denest-top)) 
			  &body body)
  `(if (not ,cond) (return-from ,to ,return) (progn ,@body)))

(synonym :until :when-return)
(synonym :while :unless-return :find)

(def-denest-macro :next-returns ((&optional (block 'denest-top))
				 &body body)
  "Make the next statement be what is returned. (Last come first served)
`(return-from ,block (progn ,@body)))"
  `(return-from ,block (progn ,@body)))

(def-denest-macro :return-this ((value &optional (block 'denest-top))
				&body body)
  "Make the next value returned. (Last come first served)
  `(progn ,@body (return-from ,block ,value))"
  `(progn ,@body (return-from ,block ,value)))

;;Some iterators.
(def-denest-macro :integer-interval ((i from to &optional (by 1))
				     &body body)
  "Does body with i on the interval."
  (let ((end (gensym)))
    `(let ((,end ,to))
       (do ((,i ,from (+ ,i ,by)))
	   ((>= ,i ,end) nil)
	 ,@body))))

(def-denest-macro :integer-block ((&rest block) &body body)
  "A block of integers."
  (if (null block)
    `(progn ,@body)
    `(use-denest-macro :integer-interval ,(car block)
       (use-denest-macro :integer-block (,@(cdr block)) ,@body))))

(def-denest-macro :on-vector
    ((el vector &key (vect-gs (gensym)) (from 0) (to `(length ,vect-gs))
	 (i (gensym))) &body body)
  "Iterates on vector. TODO make like :on-list"
  `(let ((,vect-gs ,vector))
     (use-denest-macro :integer-interval (,i ,from ,to)
       (symbol-macrolet ((,el (aref ,vect-gs ,i)))
	 ,@body))))

(def-denest-macro :on-list ((&rest els) &body body)
  "Iterate on list, or on multiple lists. Elements in form (el list).
Symbols are symbol-macrolets such that they're setf-able."
  (let ((gs (mapcar (lambda (el) (declare (ignore el)) (gensym)) els)))
    `(do (,@(mapcar (lambda (el g) `(,g ,(cadr el) (cdr ,g))) els gs))
	 ((or ,@(mapcar (lambda (g) `(null ,g)) gs)) (values))
       (symbol-macrolet (,@(mapcar (lambda (el g) `(,(car el) (car ,g)))
				   els gs))
	 ,@body))))

(def-denest-macro :on-hash ((key element hash) &body body)
  "Iterate hash table."
  `(maphash (lambda (,key ,element)
	      ,@body) ,hash))

;;Other

(def-denest-macro :firstly ((&body do) &body body)
  "Do something at the start. ]prog ,@do ,@body)"
  `(progn ,@do ,@body))

(def-denest-macro :lastly ((&body do) &body body)
  "Do something at the end, skipped if returned in some way.
 (progn ,@body)"
  `(progn ,@body ,@do))

(def-denest-macro :cond ((&rest clauses) &body body)
  "Uses COND, the body is that t condition."
  `(cond ,@clauses (t ,@body)))

;;Very basic stuff, just to make writing it shorter.

(def-denest-macro :slots ((&rest slots) obj &body body)
  "With-slots denest variant, just to save a few characters."
  `(with-slots (,@slots) ,obj ,@body))

(def-denest-macro :mval ((&rest values) input &body body)
  "Multiple-value-bind variant, just to save a few characters."
  `(multiple-value-bind (,@values) ,input ,@body))

(def-denest-macro :lval ((&rest args) list &body body)
  "destructuring-bind variant, just to save a few characters."
  `(multiple-value-bind (,@args) ,list ,@body))

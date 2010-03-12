;;
;; Copyright (c) 2009, Jasper den Ouden <o.jasper@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.
;;

(cl:in-package :cl-user)

(defpackage :denest
  (:use :common-lisp)
  (:export denest denest*
	   *denest-macros* def-denest-macro use-denest-macro
	   denest-ret finish leave
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

(defmacro denest-ret (&body body)
  "Marks where to go back to when the macro returns.
 Only thing that might ever need changing in macros. Doesn't affect normal\
 operation of macro."
  `(block denest-prev-ret ,@body))

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

(defmacro use-denest-macro (name &rest args)
  "Applies a denest macro."
  (funcall (gethash name *denest-macros*) args))

(defmacro denest-raw (&rest args)
  "Raw version of denest. Doesn't add the blocks and macrolets."
  (cond
    ((null (cdr args))
     (if (keywordp (caar args))
       `(use-denest-macro ,@(car args))
       (car args)))
    ((keywordp (caar args))
     (funcall (gethash (caar args) *denest-macros*)
	      `(,@(cdar args) (denest-raw ,@(cdr args)))))
    (t
     `(,@(car args)
	 (denest-raw ,@(cdr args))))))

(defmacro denest (&rest args)
  "Un-nests stuff, reconstructs macros in order with the body at the end.
Best and easiest description is: (But the actual version has some more 
 (defmacro denest (&rest args)
   (if (null (cdr args))
     (car args)
     `(,@(car args)
       (denest ,@(cdr args)))))"
  `(block denest
     (block denest-prev-ret
       (macrolet ((finish ()
		    '(return-from denest-prev-ret))
		  (leave (returning)
		    `(return-from denest ,returning)))
	 (denest-raw ,@args)))))

(defmacro denest* (&rest arguments)
  "denest, but adds some parenthesis so you don't have to write them. 
Has significant disadvantage; can't do stuff where the body location isn't\
 entirely indicated by the parenthesis, like in WITH-SLOTS,\
 DESTRUCTURING-BIND, MULTIPLE-VALUE-BIND."
  `(denest ,@(mapcar (lambda (a)
		       (destructuring-bind (name &rest args) a
			 `(,name (,@args)))) arguments)))

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
       (denest-ret ,@body))
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
	 (denest-ret ,@body)))
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

;;Returning/finishing.
(def-denest-macro :return-from (from returned &body body)
  "Coerce what is being returned. If using with denest-macro, note which\
 variables you do and don't have.
Needs a block to return to, 'denest is always the top block."
  (when (and (listp returned) (listp (car returned)))
    (setf returned (car returned)))
  `(progn (block denest-prev-ret ,@body)
	  (return-from ,from ,returned)))

(def-denest-macro :return (returned &body body)
  "Returns to the top block (The block DENEST.)"
  `(use-denest-macro :return-from denest ,returned ,@body))

(defmacro return-accumulate ((&rest accumulation-manners) &body body)
  "Produces accumulation into different values-output..
Must given accumulation-manners given in same way as denest ones, they must\
 have form (macro-name (something accumulating-variable ...) ...)"
  (denest
    (collecting (nil vars col-var)) ;TODO Unreachable code?
    (collecting (nil accum col-accum))
    (:return `(denest ,@accum
		(:return (values ,@vars))
		(progn ,@body)))
    (dolist (a accumulation-manners)
      (cond
	((listp a)
	 (destructuring-bind (name (&optional initial var &rest more)
				   &rest rest) a
	   (declare (ignore more))
	   (cond
	     (var (col-var var)
		  (col-accum a))
	     (t
	      (let ((var (gensym)))
		(col-var var)
		(col-accum `(,name (,initial ,var) ,@rest)))))))
	(t
	 (col-var a))))))

(defmacro return-accumulate*((&rest accumulation-manners) &body body)
  "Same as return-accumulate, but allows for less parentheses."
  `(return-accumulate (,@(mapcar (lambda (a) (if (listp a)
					       `(,(car a) (,@(cdr a))) a))
				 accumulation-manners)) ,@body))

(def-denest-macro :when-return (cond returned &body body)
  "Return when condition true. Can be used to return something when you\
 find something."
  `(if ,cond (return-from denest ,returned) (progn ,@body)))

(def-denest-macro :unless-return (cond returned &body body)
  "Return when condition false. Can be used to return something when you\
 find something."
  `(if (not ,cond) (return-from denest ,returned) (progn ,@body)))

(def-denest-macro :find (cond returned &body body)
  "Renaming of when-return so that people will find that you can use it\
 to find stuff."
  `(use-denest-macro :when-return ,cond ,returned ,@body))

;;Finishing.
(def-denest-macro :until ((cond &optional (to 'denest-prev-ret)) &body body)
  `(if ,cond (return-from ,to) (progn ,@body)))

(def-denest-macro :while ((cond &optional (to 'denest-prev-ret)) &body body)
  `(if (not ,cond) (return-from ,to) (progn ,@body)))

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
  "Do something at the start."
  `(progn ,@do ,@body))

(def-denest-macro :lastly ((&body do) &body body)
  "Do something at the end, skipped if returned in some way."
  `(progn ,@body ,@do))

(def-denest-macro :cond ((&rest clauses) &body body)
  "Uses COND, the body is that t condition."
  `(cond ,@clauses (t ,@body)))

(def-denest-macro :* ((name &rest args) &body body)
  "Little macro to be able to give arguments the regular way in DENEST*"
  `(,@(when (keywordp name) '(use-denest-macro)) ,name ,@args ,@body))

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

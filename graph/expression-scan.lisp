
(cl:in-package :cl)

(defpackage :expression-scan
  #+lispworks (:import-from #:lispworks #:compiler-let)
  (:use :cl :generic :expression-hook :package-stuff)
  (:export add-scanner-fun add-scanner
	   *additional-scan* access-result
	   scanner-state scan-expression-hook
	   track-fun track-var
	   type name args init dep var-dep
	   scan-single-expression)
  (:documentation 
   "Can create and use expression-hook (macroexpand-dammit from John\
 Fremlin modified) to obtain information about code.
Any s-expression can be tracked."))

(in-package :expression-scan)

(defvar *fun-scan* (make-hash-table)
  "Scanners for the different macros/functions under observation.")
(defparameter *additional-scan* (list)
  "List of functions for scanning besides every expression.\
 (Rather then fun-scan, elements of which only scan specific macros.")

(defun add-scanner-fun (for-name scan-function)
  "Adds a scanner for a macro/function."
  (declare (type function scan-function))
  (setf (gethash for-name  *fun-scan*) scan-function))

(defmacro add-scanner (for-name (&rest args) &body body)
  "Add scanner-fun, but does the creation of function for you."
  (with-gensyms (code)
    `(flet ((scanning-fun (,code)
	      ,@(when (stringp (car body)) (list(car body)))
	      (destructuring-bind (,@args) ,code
		,@body)))
       (add-scanner-fun ',for-name #'scanning-fun))))
	      
(defparameter *scan-result* (make-hash-table :test 'equalp)
  "Lists result by name.")

(defun hash-name-result (fun-name name)
  "Produces 'name' used in hash."
  (vector fun-name name))

(defun access-result (fun-name name)
  "Accesses result of scan of macro/function."
  (gethash (hash-name-result fun-name name) *scan-result*))
(defun (setf access-result) (to fun-name name)
  "See non-setf version."
  (setf (gethash (hash-name-result fun-name name) *scan-result*) to))

(defvar *in-fun* nil)
(defvar *ignore-packages* (list :sb-impl :sb-int :sb-c :sb-pcl :sb-kernel))

;;The scanner hook.
(defun scan-expression-hook (expr)
  "The expression hook of the scanner."
  (dolist (fn *additional-scan*)
    (funcall fn expr))
  (if-use
   (when (and expr (listp expr))
     (when-let fn (gethash (car expr) *fun-scan*)
       (funcall fn expr)))
   (expand-hook expr)))

;;Scanning stuff.
(defun scan-file (stream)
  "Scans a file as source code in order to document it."
  (let ((*expression-hook* #'scan-expression-hook)
	(*expression-hook-state* (make-instance 'scanner-state)))
    (do ((read nil (read stream nil 'end-of-file)))
	((eql read 'end-of-file) nil)
      (eval(hook-expand read)))))

(defun scan-loadfile (stream)
  "Scans a file with a bunch of loads in it."
  (do ((read nil (read stream nil 'end-of-file)))
      ((eql read 'end-of-file) nil)
    (when (eql (car read) 'load)
      (scan-file (cadr read)))))

(defun scan-single-expression (expr)
  (let ((*expression-hook* #'scan-expression-hook)
	(*in-fun* nil))
    (eval (hook-expand expr))))

;;Data tracker for functions and macros.
;; Note the convention is to not track data already readily available, 
;; like documentation strings.rc[
(defclass track-fun ()
  ((type :initarg :type :type symbol)
   (name :initarg :name :type symbol)
   (args :initarg :args :type list)
   
   (dep :initarg :dep :initform nil :type list
	:documentation "What functions/macros it depends on.")
   (var-dep :initarg :var-dep :initform nil :type list
	    :documentation "Variables it depends on."))
  (:documentation
   "Structure to contain information on functions and macros."))

(defmacro add-dep (dep-list dep)
  `(unless (find ,dep ,dep-list)
     (push ,dep ,dep-list)))

(flet ((fun-scanner (expr)
	 "Function to scan function/macro."
	 (destructuring-bind (type name (&rest args) &body body) expr
	   (declare (ignore body))
	   (when (access-result type name)
	     (warn "Function ~D encountered twice." name))
	   (let ((*in-fun*
		  (setf (access-result type name)
			(make-instance 'track-fun :type type
						  :name name :args args))))
	     (expand-hook expr)))))
  (add-scanner-fun 'defun #'fun-scanner)
  (add-scanner-fun 'defmacro #'fun-scanner))

(flet ((additional-scanner-fun (expr)
	 "Scans for used variables/functions to find "
	 (cond
	   ((or (not expr) (not *in-fun*))
	    nil)
	   ((listp expr) ;Register function/macro useages.
	    (unless (listfind-package (car expr) *ignore-packages*)
	      (add-dep (slot-value *in-fun* 'dep) (car expr))))
	   ((symbolp expr) ;Register var/parameter useages. 
	    (when (and (not (listfind-package expr *ignore-packages*))
		       (or (access-result 'defvar expr)
			   (access-result 'defparameter expr)))
	      (add-dep (slot-value *in-fun* 'var-dep) expr))))))
  (push #'additional-scanner-fun *additional-scan*))

;;Data tracker for variables.
(defclass track-var ()
  ((type :initarg :type :type symbol)
   (name :initarg :name :type symbol)
   (init :initarg :init))
  (:documentation "Structure containing information on macros."))

(flet ((var-scanner (expr)
	 "Function to scan variable creation by defvar/defparameter."
	 (destructuring-bind (type name &optional init doc-str) expr
	   (declare (ignore doc-str))
	   (setf (access-result type name)
		 (make-instance 'track-var
				:type type :name name :init init)))
	 nil))
  (add-scanner-fun 'defvar #'var-scanner)
  (add-scanner-fun 'defparameter #'var-scanner))

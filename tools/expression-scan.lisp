;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl)

;TODO how is it printing?!?!?
(defpackage :expression-scan
  #+lispworks (:import-from #:lispworks #:compiler-let)
  (:use :cl :generic :package-stuff :expression-hook)
  (:nicknames :expr-scan)
  (:export add-scanner-fun def-scanner ;TODO are all these needed?
	   fun-scanner
	   *additional-scan* access-result *scan-result*
	   
	   scan-expression-hook scan-macro-hook
	   
	   scan-expr scan-file scan-loadfile scan-system
	 ;Some of the scan data that come with it
	   track-fun track-var
	   track-form track-generic track-method
	   type name args init fun-dep var-dep flet-dep var-dep other
	   
	   add-scanner def-scanner expr)
  (:documentation 
   "Can create and use expression-hook to obtain information about code.
Any s-expression can be tracked. (So macros and functions can be tracked.)
"))

;;TODO reader-macro to try fish out some comments?

(in-package :expression-scan)

(defvar *fun-scan* (make-hash-table)
  "Scanners for the different macros/functions under observation.")
(defparameter *additional-scan* (list)
  "List of functions for scanning besides every expression.\
 (Rather then fun-scan, elements of which only scan specific macros.")

(defvar *reading-myself* nil)

(defgeneric name (obj) (:documentation "Gets name of object."))

(defun add-scanner-fun (for-name scan-function)
  "Adds a scanner for a macro/function."
  (declare (type function scan-function))
  (setf (gethash for-name *fun-scan*) scan-function))

(defmacro def-scanner (for-name (&rest args) &body body)
  "Add-scanner-fun, but does the creation of function for you.
Note that the variable expr contains the whole expression."
  `(flet ((scanning-fun (expr)
	    ,@(when (stringp (car body)) (list(car body)))
	    (destructuring-bind (,@args) (cdr expr)
		,@body)))
     (add-scanner-fun ',for-name #'scanning-fun)))
	      
(defvar *scan-result* (make-hash-table :test 'equalp)
  "Lists result by name.")

(defun access-result (fun-name name)
  "Accesses result of scan of macro/function.
Providing a list of fun-names will search them in sequence."
  (cond
    ((null fun-name) nil)
    ((listp fun-name)
      (if-use (access-result (car fun-name) name)
	      (access-result (cdr fun-name) name)))
    (t
     (gethash (vector fun-name name) *scan-result*))))

(defun (setf access-result) (to fun-name name)
  "See non-setf version."
  (setf (gethash (vector fun-name name) *scan-result*) to))

(defvar *ignore-packages* (list :sb-impl :sb-int :sb-c :sb-pcl :sb-kernel))

(def-scanner in-package (name)
  (if-let to-package (find-package name)
    (setq *package* to-package)
    (progn
      (warn "Couldn't find package for ~a. It might not have been loaded.
Discontinued scan."
	    name)
      (setq expr-hook::*discontinue* t)))
  expr)

;;The scanner hook. ;TODO scan asdf stuff.
(defun scan-expression-hook (expr)
  "The expression hook of the scanner."
  (dolist (fn *additional-scan*)
    (funcall fn expr))
  (if-use
   (when (and expr (listp expr)) ;See if any trackers for it.
     (when-let fn (gethash (car expr) *fun-scan*)
       (funcall fn expr))) ;Trackers need to expand-hook, (otherwise stops.)
   (expand-hook expr))) ;Otherwise expand-hook itself.

(defun scan-macrohook (expander form env)
  "Function for in *macroexpand-hook*, doesn't make a nearly as complete\
 scan, but will work with regular loading."
  (when-let fn (gethash (car form) *fun-scan*)
    (funcall fn form))
  (funcall expander form env))

;;Scanning stuff.
(defun scan-file (stream ;This aught to be default, right?
		  &key (*package* (find-package :cl-user))
		       (expression-hook #'scan-expression-hook))
  "Scans a file as source code in order to document it."
  (if (or (stringp stream) (pathnamep stream))
    (with-open-file (stream stream)
      (scan-file stream
		 :*package* *package* :expression-hook expression-hook))
    (let ((expr-hook::*discontinue* nil)
	  (*expression-hook* expression-hook)
	  (*reading-myself* t))
      (do ((read nil (read stream nil 'end-of-file)))
	  ((or (eql read 'end-of-file)
	       expr-hook::*discontinue*) nil)
	(expand read)))))

(defun scan-loadfile (stream)
  "Scans a file with a bunch of loads in it."
  (scan-file stream
    :expression-hook
    (lambda (expr)
      (if (and (listp expr) (eql (car expr) 'load))
	(scan-file (cadr expr) :*package* (find-package :cl-user))
	(expand-hook expr)))))

(defun scan-expr (expr)
  "Scans a single expression."
  (let ((*expression-hook* #'scan-expression-hook)
	(*in-funs* nil))
    (expand expr)))

(defclass track-form ()
  ((form :initarg :form :type list)))

(defmethod name ((tf track-form))
  (cadr (slot-value tf 'form)))

(defun form-scanner (expr &key (class 'track-form))
  (setf (access-result (car expr) (cadr expr))
	(make-instance class :form expr))
  (expand-hook expr))

(def-scanner defpackage (name &rest rest)
  (let ((name (typecase name ;Always in keyword.
		(string (intern name :keyword))
		(symbol (intern (symbol-name name) :keyword)))))
    (form-scanner `(defpackage ,name ,@rest)))
  expr)

;;Data tracker for functions and macros.
;; Note the convention is to not track data already readily available, 
;; like documentation strings.rc
(defclass track-fun ()
  ((type :initarg :type :type symbol)
   (name :initarg :name :type symbol :reader name)
   (args :initarg :args :type list)
   
   (in-funs :initarg :in-funs :type list
     :documentation "Which functions/macros created it.")
   (fun-dep :initarg :fun-dep :initform nil :type list
	    :documentation "What functions/macros it depends on.")
;   (flet-dep :initarg :flet-dep :initform nil :type list
;     :documentation "Flet/macrolets it depends on.")
   (var-dep :initarg :var-dep :initform nil :type list
     :documentation "Assoc list with variables it depends on, and origin."))
  (:documentation
   "Structure to contain information on functions and macros."))

(defun fun-like-scanner
    (type name args &optional (expand-further (lambda ())))
  (let ((fun (make-instance 'track-fun :name name :args args
		:type type :in-funs *in-funs*)))
    (let ((*in-funs* (cons fun *in-funs*)))
      (values (funcall expand-further) fun))))

(defun fun-scanner (expr)
  "Function to scan function/macro."
  (destructuring-bind (type name args &rest ignore) expr
    (declare (ignore ignore))
    (multiple-value-bind (result fun)
	(fun-like-scanner type name args
			  (lambda () (expand-hook expr)))
      (setf (access-result type name) fun)
      result)))

(add-scanner-fun 'defun #'fun-scanner)
(add-scanner-fun 'defmacro #'fun-scanner)

(flet ((additional-scanner-fun (expr)
	 "Scans for used variables/functions to find "
	 (when (or (not expr) (null *in-funs*))
	   (return-from additional-scanner-fun))
	 (when-let tracker
	     (if-use (access-result 'defmacro (car *in-funs*))
		     (access-result 'defun (car *in-funs*))
		     (access-result 'defvar (car *in-funs*))
		     (access-result 'defparameter (car *in-funs*)))
	   (with-slots (var-dep fun-dep) tracker
	     (cond
	       ((symbolp expr) ;Register var/parameter useages. 
		(when (and (not (assoc expr *eh-sym-macs*))
			   (or (access-result 'defvar expr)
			       (access-result 'defparameter expr))
			   (find expr var-dep))
		  (push expr var-dep)))
	       ((listp expr) ;Register function/macro useages.
;External flets and lets don't count. TODO They should..
		(unless (or (find expr *eh-funs*)
			    (find expr *eh-macs*)
			    (find (car expr) fun-dep))
		  (push (car expr) fun-dep))))))))
  (push #'additional-scanner-fun *additional-scan*))

(defclass track-generic (track-form)
  ((methods :initarg :methods :initform nil :type list)
   (args :initarg :args :type list))
  (:documentation "Tracks method generic declarations."))

(def-scanner defgeneric (name &rest rest)
  (declare (ignore rest))
  ;Make it, if methods already existed, incorporate.
  (setf (access-result 'defgeneric name)
	(make-instance 'track-generic :form expr
	  :methods (when-let prev (access-result 'defgeneric name)
		     (slot-value prev 'methods))))
  expr)

(defclass track-method (track-fun)
  ((way :initarg :way :initform nil :type symbol)))

(def-scanner defmethod (name way/args &optional args/dstr
			     dstr/body &body body)
  "Scans a method. Notably not in a new entry, lists on the defgeneric."
  (multiple-value-bind (args dstr body)
      (cond
	((listp way/args)
	 (values way/args args/dstr (cons dstr/body body)))
	((listp args/dstr)
	 (values args/dstr dstr/body body))
	(t                 (error "")))
    (let((gen
	  (if-use ;Automatically makes a generic if not scanned.
	   (access-result 'defgeneric name)
	   (setf (access-result 'defgeneric name)
		 (make-instance 'track-generic
		   :form `(defgeneric ,name (,@(mapcar #'delist args))
			    ,@(when dstr `((:documentation ,dstr)))))))))
     ;Push the method.
      (multiple-value-bind (result fun)
	  (fun-like-scanner 'defmethod name args
			    (lambda () (expand-hook expr)))
	(push (change-class fun 'track-method
			    :way (when (symbolp way/args) way/args))
	      (slot-value gen 'methods))
	result))))

;;Data tracker for variables.
(defclass track-var (track-form)
  ((fun-dep :initform nil :type list)
   (var-dep :initform nil :type list))
  (:documentation "Structure containing information on macros.
fun-dep and var-dep for initform!"))

(flet ((var-scanner (expr)
	 "Function to scan variable creation by defvar/defparameter."
	 (destructuring-bind (type name &rest rest) expr
	   (declare (ignore rest))
	   (setf (access-result type name)
		 (make-instance 'track-var :form expr)))
	 (expand-hook expr)))
  (add-scanner-fun 'defvar #'var-scanner)
  (add-scanner-fun 'defparameter #'var-scanner))

;TODO better class scanning?
(add-scanner-fun 'defclass #'form-scanner)
(add-scanner-fun 'defstruct #'form-scanner)

;;Scanning asdf systems; load the asd file.
(defvar *follow-asdf-systems* :also-load)

(defun scan-asdf-components (components)
  (dolist (component components)
    (case (car component)
      (:file
       (scan-file (format nil "~a.lisp" (cadr component))
		  :expression-hook *expression-hook*))
      (:module
       (let ((*default-pathname-defaults*
	      (pathname (format nil "~a~a/"
		  (directory-namestring *default-pathname-defaults*)
		  (cadr component)))))
	 (scan-asdf-components (getf (cddr component) :components)))))))

(def-scanner asdf::defsystem (system-name &rest info)
  (setf (access-result 'asdf:defsystem system-name)
	(make-instance 'track-form :form expr))
  ;If we know where we are, and are to folow:
  (when *follow-asdf-systems*
    (let ((*default-pathname-defaults* 
	   (if-use (unless *reading-myself*
		     *load-pathname*)
		   *default-pathname-defaults*)))
      (print system-name)
      (when (asdf:find-system system-name nil)
	(when :also-load
	  (asdf:oos 'asdf:load-op system-name))
     ;We only need to scan the files.
     ;TODO doesn't work if subcomponents.
	(scan-asdf-components (getf info :components)))))
  expr)

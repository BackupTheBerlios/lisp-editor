
(cl:in-package :cl)

;TODO how is it printing?!?!?
(defpackage :expression-scan
  #+lispworks (:import-from #:lispworks #:compiler-let)
  (:use :cl :generic :plist-slot :package-stuff :expression-hook)
  (:nicknames :expr-scan)
  (:export add-scanner-fun def-scanner
	   fun-scanner
	   *additional-scan* access-result *scan-result*
	   scan-expression-hook
	   scan-expr scan-file scan-loadfile scan-system
	 ;Some of the scanners that come with it
	   track-fun track-var ;Functions and macros, variables.
	   type name args init fun-dep var-dep flet-dep var-dep other)
  (:documentation 
   "Can create and use expression-hook to obtain information about code.
Any s-expression can be tracked. (So macros and functions can be tracked."))

;;TODO reader-macro to try fish out some comments?

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

(defmacro def-scanner (for-name (&rest args) &body body)
  "Add-scanner-fun, but does the creation of function for you."
  (with-gensyms (code)
    `(flet ((scanning-fun (,code)
	      ,@(when (stringp (car body)) (list(car body)))
	      (destructuring-bind (,@args) ,code
		,@body)))
       (add-scanner-fun ',for-name #'scanning-fun))))
	      
(defparameter *scan-result* (make-hash-table :test 'equalp)
  "Lists result by name.")

(defun access-result (fun-name name)
  "Accesses result of scan of macro/function."
  (cond
    ((null fun-name) nil)
    ((listp fun-name)
      (if-use (access-result (car fun-name) name)
	      (access-result (cdr fun-name) name)))
    (t (gethash (vector fun-name name) *scan-result*))))
(defun (setf access-result) (to fun-name name)
  "See non-setf version."
  (setf (gethash (vector fun-name name) *scan-result*) to))

(defvar *ignore-packages* (list :sb-impl :sb-int :sb-c :sb-pcl :sb-kernel))

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

;;Scanning stuff.
(defun scan-file (stream
		  &key (*package* *package*)
		       (expression-hook #'scan-expression-hook))
  "Scans a file as source code in order to document it."
  (if (or (stringp stream) (pathnamep stream))
    (with-open-file (stream stream)
      (scan-file stream
		 :*package* *package* :expression-hook expression-hook))
    (let*((*expression-hook*
	   (lambda (expr)
	     (if (and (listp expr) (eql (car expr) 'in-package))
	       (setq *package* (find-package (cadr expr)))
	       (funcall expression-hook expr)))))
      (do ((read nil (read stream nil 'end-of-file)))
	  ((eql read 'end-of-file) nil)
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

;;Data tracker for functions and macros.
;; Note the convention is to not track data already readily available, 
;; like documentation strings.rc
(defclass track-fun (plist-slot)
  ((type :initarg :type :type symbol)
   (name :initarg :name :type symbol)
   (args :initarg :args :type list)
   
   (in-funs :initarg :in-funs :type list
     :documentation "Which functions/macros created it.")
   (fun-dep :initarg :fun-dep :initform nil :type list
	    :documentation "What functions/macros it depends on.")
   (flet-dep :initarg :flet-dep :initform nil :type list
     :documentation "Flet/macrolets it depends on.")
   (var-dep :initarg :var-dep :initform nil :type list
     :documentation "Assoc list with variables it depends on, and origin."))
  (:documentation
   "Structure to contain information on functions and macros."))

(defun fun-scanner (expr)
  "Function to scan function/macro."
  (destructuring-bind (type name (&rest args) &body body) expr
    (declare (ignore body))
    (when (access-result type name)
      (warn "Function ~D encountered twice." name))
    (setf (access-result type name)
	  (make-instance 'track-fun :type type :in-funs *in-funs*
			 :name name :args args))
    (expand-hook expr)))

(add-scanner-fun 'defun #'fun-scanner)
(add-scanner-fun 'defmacro #'fun-scanner)

(flet ((additional-scanner-fun (expr)
	 "Scans for used variables/functions to find "
	 (when (or (not expr) (null *in-funs*))
	   (return-from additional-scanner-fun))
	 (when-let tracker (if-use (access-result 'defmacro (car *in-funs*))
				   (access-result 'defun (car *in-funs*)))
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

;;Data tracker for variables.
(defclass track-var (plist-slot)
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
	 (expand-hook expr)))
  (add-scanner-fun 'defvar #'var-scanner)
  (add-scanner-fun 'defparameter #'var-scanner))

;;TODO defclass, defstruct. (And then also for autodoc.)

;;Scanning systems.
(when (find-package :asdf)
  
  (defun scan-system
      (sys
       &key (verbose t) version proclamations
       (load-hook
	(lambda (sys)
	  (declare (ignore sys))
	  (values verbose version proclamations)))
       (expression-hook #'scan-expression-hook) (recurse-cnt 0))
    (when (symbolp sys) ;Turn keywords into names.
      (setf sys (asdf:find-system sys)))
   ;First assure that it is there.(We need to macros to actually be loaded.)
    (multiple-value-bind (verbose version proclamations)
	(funcall load-hook sys)
      (asdf:operate 'asdf:load-op sys
	:verbose verbose :version version :proclamations proclamations))
   ;Scan it, gathering packages as we go.
    (let*(packages 
	  (*expression-hook*
	   (lambda (expr)
	     (cond
	       ((and (listp expr) (eql (car expr) 'defpackage))
		(let ((pkg (find-package (cadr expr))))
		  (pushnew pkg packages)
		  (setq *package* pkg)))
	       (t
		(funcall expression-hook expr)))))
	  (recurse-to (when (> recurse-cnt 0)
			(cdr (assoc 'asdf:compile-op
				    (asdf:component-depends-on
				     'asdf:compile-op sys)))))
	  (*default-pathname-defaults* (pathname "/"))
	  (pathname 
	   (format nil "~{~a/~}" (cdr(pathname-directory
				      (asdf:component-pathname sys)))))
	  (filenames (cdr (assoc 'asdf:compile-op
				 (asdf:component-depends-on
				  'asdf:load-op sys)))))
				 
      (dolist (file filenames)
	(with-open-file (stream (format nil "~D~D.lisp" pathname file)
				:if-does-not-exist nil)
	  (when stream
	    (do ((read nil (read stream nil 'end-of-file)))
		((eql read 'end-of-file) nil)
	      (expand read)))))
    ;Recurse if asked, return packages.
      (cons packages ;You can tell different levels apart.
	    (mapcan (curry #'scan-system
		      :load-hook load-hook :recurse-cnt (- recurse-cnt 1))
		    recurse-to))))
  
  ) ;\(when (find-package :asdf)


;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :expression-scan
  (:use :common-lisp :alexandria :generic :denest
	:package-stuff :path-stuff
	:expression-hook)
  (:nicknames :expr-scan)
  (:export
 ;Functions to scan things.
   scan-expr scan-file scan-loadfile
 ;The hooks being scanned with
   scan-expression-hook scan-macro-hook
 ;Defining scanners.
   add-scanner-fun add-scanner def-scanner expr *additional-scan* 
 ;Accessing information.
   access-result *package-list* list-packages-below-path track-data
 ;Some of the classes used to put scan data in.
   base-track typed-track depend-track
   track-fun track-var
   track-form track-generic track-method
   track-package
 ;Getting data out of that.
   type name args init fun-dep var-dep flet-dep var-dep other
   package-scan-list)
  (:documentation "Can create and use expression-hook to obtain\
 information about code. Any s-expression can be tracked.
 (So macros and functions can be tracked.)"))

(in-package :expression-scan)

(defvar *overrider* nil "If a scanner is overriding sub-scanners.")

;;TODO reader-macro to try fish out some comments?

;;Current file.
(defvar *cur-file* nil
  "Current file being scanned.")
(defvar *cur-path* nil
  "Current path being scanned.")

(defun cur-file ()
  "Attempts to get current file being scanned."
  (values (or *cur-file* *load-truename* *compile-file-truename*)
	  (or *cur-path* *load-pathname* *compile-file-pathname*)))

;;Some vars
(defvar *cur-package* nil)

(defvar *reading-myself* nil) ;;TODO unclear.

(defgeneric name (obj) (:documentation "Gets name of object."))

(defmethod name ((null null)))

;;Scanners
(defvar *scanned-forms* (list) "Names of the scanned forms.")

(defvar *fun-scan* (make-hash-table)
  "Scanners for the different macros/functions under observation.")

(defparameter *additional-scan* (list)
  "List of functions for scanning besides every expression.\
 (Rather then fun-scan, elements of which only scan specific macros.")

(defun add-scanner-fun (for-name scan-function)
  "Adds a scanner for a macro/function."
  (declare (type function scan-function))
  (push for-name *scanned-forms*)
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
  "Lists result by name. If you clear it.")

;;Accessing result.
(defun access-key (fun-name name)
  (case fun-name
    (defpackage (vector fun-name (package-keyword name)))
    (t          (vector fun-name name))))

(defun access-result (fun-name name)
  (typecase fun-name
    (null)
    (list (dolist (f-name fun-name)
	    (when-let ((result (access-result f-name name)))
	      (return result))))
    (t    (gethash (access-key fun-name name) *scan-result*))))

(defun (setf access-result) (to fun-name name)
  "See non-setf version. Be warned, will register with the package too!"
  (when *cur-package*
    (pushnew to (slot-value *cur-package* 'scan-list) :test #'equalp))
  (setf (gethash (access-key fun-name name) *scan-result*) to))

(defun list-packages-below-path (path)
  "Lists all the packages that are in the path or in subdirectories\
 thereof."
  (remove-if-not
   (lambda (track)
     (with-slots (paths) track
       (find-if (compose (curry #'sub-path-of path) #'namestring) paths)))
   (copy-list *package-list*)))

;;The scanner hook. ;TODO scan asdf stuff.
(defun scan-expression-hook (expr)
  "The expression hook of the scanner."
  (dolist (fn *additional-scan*)
    (funcall fn expr))
  (when (and expr (listp expr)) ;See if any trackers for it.
    (when-let (fn (gethash (car expr) *fun-scan*))
      (funcall fn expr)))
  (expand-hook expr))

(defun scan-macrohook (expander form env)
  "Function for in *macroexpand-hook*, doesn't make a nearly as complete\
 scan, but will work with regular loading."
  (when-let (fn (gethash (car form) *fun-scan*))
    (funcall fn form))
  (funcall expander form env))

(defun check-symbol (symbol)
  "Checks if the symbol needs to be added as dependency of the\
 package."
  (denest
   (when (and symbol (symbolp symbol) *cur-package*))
   (when-let ((pkg (symbol-package symbol))))
   (let ((pkg (intern (package-name pkg) :keyword))))
   (with-slots (uses also-uses) *cur-package*)
   (unless (or (find pkg *ignore-packages*) (find pkg uses)
	       (eql pkg :keyword))
     (pushnew pkg also-uses))))

(defun check-top (top)
  (typecase top
    (symbol (check-symbol top))
    (list   (check-top (car top))
	    (check-top (cdr top)))))

(defvar *load-first* nil "Whether to load a file first.")
(defvar *judge* (constant t) "Judges whether to scan a file.")

;;Scanning stuff. ;;TODO should some of it be other package?
(defun scan-file (from ;This aught to be default, right?
		  &key (*package* (find-package :cl-user))
		       (*expression-hook* #'scan-expression-hook)
		       (*load-first* *load-first*)
		       (*judge* *judge*))
  "Scans a file as source code in order to document it.
'From' can be a string, pathname, list or stream. If it is a directory, 
 it will read all the '.lisp' files in it."
  (typecase from
    ((or string pathname)
     (if (cl-fad:directory-pathname-p from)
       (scan-file (remove-if-not
		   (lambda (file)
		     (unless (funcall *judge* file)
		       (or (cl-fad:directory-pathname-p file)
			   (case (aref (file-namestring file) 0)
			     ((#\. #\#) nil)
			     (t
			      (string= (path-stuff:file-extension file)
				       ".lisp"))))))
		   (cl-fad:list-directory from)))
       (let ((*cur-file* from)
	     (*cur-path* *default-pathname-defaults*))
	 (when *load-first*
	   (load from))
	 (with-open-file (stream from)
	   (scan-file stream)))))
    (list
     (mapcar #'scan-file from))
    (stream
     (let ((expr-hook::*discontinue* nil)
	   (*reading-myself* t))
       (do ((read nil (read from nil 'end-of-file)))
	   ((or (eql read 'end-of-file)
		expr-hook::*discontinue*) nil)
	 (check-top read)
	 (expand read))))))

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
    (check-top expr)
    (expand expr)))

;;Some aspects of tracking.
(defclass base-track ()
  ((keywords :initarg :keywords :initform nil :type list)
   (overrider :initform nil :initarg :overrider)
   (from-package :initform *cur-package*
     :documentation "Package it originates from, in case that isn't clear\
 from expression.")))

(defclass typed-track ()
  ((type :initarg :type :initform nil)))

(defclass depend-track ()
  ((fun-dep :initarg :fun-dep :initform nil :type list
     :documentation "What functions/macros it depends on.")
;   (flet-dep :initarg :flet-dep :initform nil :type list
;     :documentation "Flet/macrolets it depends on.")
   (var-dep :initarg :var-dep :initform nil :type list
     :documentation "Assoc list with variables it depends on, and origin.")))

(defun track-data (track name)
  (getf (slot-value track 'keywords) name))
(defun (setf track-data) (to track name)
  (setf (getf (slot-value track 'keywords) name) to))

(defclass track-form (base-track)
  ((form :initarg :form :type list)))

(defmethod etype ((track track-form))
  (car (slot-value track 'form)))

(defmethod name ((tf track-form))
  (cadr (slot-value tf 'form)))

(defun form-scanner (expr &key (class 'track-form))
  (setf (access-result (car expr) (cadr expr))
	(make-instance class :form expr))
  (expand-hook expr))

;;Package stuff.
(defvar *package-list* nil
  "List of all found packages.")

(defparameter *ignore-packages* 
  (list :sb-impl :sb-int :sb-c :sb-pcl :sb-kernel :sb-loop)
  "Packages to ignore because they're boring or they're\
 implementation-fidlybits.")

(defvar *couldnt-find-package* nil "List of packages not found.")

(def-scanner in-package (name)
  (let ((name (package-keyword name)))
    (setq *cur-package* (access-result 'defpackage name))
    (unless
	(cond ((find-package name)
	       (setq *package* (find-package name)))
	      ((asdf:find-system name nil)
	       ;(asdf:oos 'asdf:load-op name) ;Try to get it.
	       (when (find-package name)
		 (setq *package* (find-package name)))))
      (warn "Couldn't find package for ~a. It might not have been\
 loaded. (May want to check the order in which you are loading the files)
Discontinued scan."
	    name)
      (pushnew name *couldnt-find-package*)
      (setq expr-hook::*discontinue* t))
    (when-let (tracker (access-result 'defpackage (intern* name :keyword)))
      (pushnew (cur-file) (slot-value tracker 'paths) :test 'equalp)))
  expr)

(defclass track-package (track-form)
  ((paths :initarg :parts :initform nil :type list
     :documentation "Paths where '(in-package' was found.")
   (uses :initform nil :type list :initarg :uses)
   (also-uses :initform nil :type list
     :documentation "Packages it uses without :use.
TODO")
   (scan-list :initform nil :type list
     :documentation "List of things scanned inside the package.")))

(def-scanner defpackage (name &rest rest)
  (let*((name (package-keyword name))
	(tracker
	 (make-instance 'track-package :form `(defpackage ,name ,@rest)
			:uses (cdr(assoc :use rest)))))
    (setf (access-result 'defpackage name) tracker)
    (setq *package-list* ;TODO a special var indicating if it is allowed.
	  (remove-if (compose (curry #'eql name) #'name) *package-list*))
    (push tracker *package-list*))
  expr)

;;Data tracker for functions and macros.
;; Note the convention is to not track data already readily available, 
;; like documentation strings.rc
(defclass track-fun (base-track depend-track)
  ((type :initarg :type :type symbol :reader etype)
   (name :initarg :name :type symbol :reader name)
   (args :initarg :args :type list)
   
   (in-funs :initarg :in-funs :type list
     :documentation "Which functions/macros created it."))
  (:documentation
   "Structure to contain information on functions and macros."))

(defun fun-like-scanner (type name args)
  (let*((fun (make-instance 'track-fun :name name :args args
	       :type type :in-funs *in-funs* :overrider *overrider*)))
    (values (if *overrider* *in-funs* (cons fun *in-funs*)) fun)))

(flet ((fun (type name args expr)
	 (multiple-value-bind (*in-funs* fun)
	     (fun-like-scanner type name args)
	   (setf (access-result type name) fun)
	   (expand-hook expr))))
  (def-scanner defun (name args &rest ignore)
    (declare (ignore ignore))
    (fun 'defun name args expr))
  (def-scanner defmacro (name args &rest ignore)
    (declare (ignore ignore))
    (fun 'defmacro name args expr)))

(defun add-dep-to-tracker (tracker expr)
  (denest
   (when tracker)
   (with-slots (var-dep fun-dep) tracker)
   (typecase expr
     (symbol ;Register var/parameter useages. 
      (when (and (not (assoc expr *eh-sym-macs*))
		 (or (access-result 'defvar expr)
		     (access-result 'defparameter expr)))
	(pushnew expr var-dep)))
     (list ;Register function/macro useages.
;External flets and lets don't count. TODO They should..
      (let ((name (car expr)))
	(unless (or (find name *eh-funs*) (find name *eh-macs*))
	  (pushnew name fun-dep)))))))

(defun additional-scanner-fun (expr)
  "Scans for used variables/functions to find "
  (unless (null expr)
    (dolist (fun *in-funs*)
      (add-dep-to-tracker
       (or (unless (or (listp fun) (symbolp fun))
	     fun) ;TODO all those derived from depend-track?
	   (access-result 'defmacro fun)
	   (access-result 'defun fun)
	   (access-result 'defvar fun)
	   (access-result 'defparameter fun)
	   (access-result 'defgeneric fun))
       expr))))
(push #'additional-scanner-fun *additional-scan*)

(defclass track-generic (track-form typed-track depend-track)
  ((methods :initarg :methods :initform nil :type list)
   (args :initarg :args :type list))
  (:documentation "Tracks method generic declarations."))

(def-scanner defgeneric (name &rest rest)
  (declare (ignore rest))
  ;Make it, if methods already existed, incorporate.
  (setf (access-result 'defgeneric name)
	(make-instance 'track-generic :form expr
	  :methods (when-let (prev (access-result 'defgeneric name))
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
    (declare (ignore body))
    (let((gen
	  (or ;Automatically makes a generic if not scanned.
	   (access-result 'defgeneric name)
	   (setf (access-result 'defgeneric name)
		 (make-instance 'track-generic
		   :form `(defgeneric ,name (,@(mapcar #'delist args))
			    ,@(when dstr `((:documentation ,dstr)))))))))
     ;Push the method.
      (multiple-value-bind (*in-funs* fun)
	  (fun-like-scanner 'defmethod name args)
	(push (change-class fun 'track-method
			    :way (when (symbolp way/args) way/args))
	      (slot-value gen 'methods))
	(expand-hook expr)))))

;;Data tracker for variables.
(defclass track-var (track-form typed-track)
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

(def-scanner declaim (&rest args)
  (dolist (a args)
    (case (car a)
      (inline
	(dolist (sym (cdr a))
	  (when-let (result
		     (access-result '(defun defgeneric defmacro) sym))
	    (setf (track-data result :inline) t))))
      (type
       (dolist (sym (cddr a))
	 (when-let (result (access-result '(defvar defparameter) sym))
	   (setf (slot-value result 'type) (cadr a)))))
      (ftype ;Note: a little code repeat here.
       (dolist (sym (cddr a))
	 (when-let (result (access-result '(defun defgeneric defmacro) sym))
	   (setf (slot-value result 'type) (cadr a)))))))	 
  (expand-hook expr))

;;Scanning asdf systems forms; load the asd file.
(defvar *follow-asdf-systems* t
  "Whether to follow the files of asdf system that is scanned.
nil: Don't follow
true: Follow, assumes the macros in the files have been executed!
:also-load: If found load it with (asdf:oos 'asdf:load-op system-name).")

(defun scan-asdf-components (components)
  (dolist (component components)
    (destructuring-bind (name value &rest rest) component
      (case name
	(:file
	 (scan-file (format nil "~a.lisp" (or (when (symbolp value)
						(string-downcase value))
					      value))))
	(:module
	 (let ((*default-pathname-defaults*
		(pathname (format nil "~a~a/"
		   (directory-namestring *default-pathname-defaults*)
		   (or (when (symbolp value) (string-downcase value))
		       value)))))
	   (scan-asdf-components (getf rest :components))))))))

(def-scanner asdf::defsystem (system-name &rest info)
  (setf (access-result 'asdf:defsystem system-name)
	(make-instance 'track-form :form expr))
  ;If we know where we are, and are to folow:
  (let ((*default-pathname-defaults* 
	 (or (unless *reading-myself*
	       *load-pathname*)
	     *default-pathname-defaults*))
	(follow
	 (and (asdf:find-system system-name nil)
	      (or (when (functionp *follow-asdf-systems*)
		    (funcall *follow-asdf-systems* system-name))
		  *follow-asdf-systems*))))
    (when (eql follow :also-load)
      (asdf:oos 'asdf:load-op system-name))
   ;We only need to scan the files.
   ;TODO doesn't work if subcomponents.
    (when follow
      (scan-asdf-components (getf info :components))))
  expr)

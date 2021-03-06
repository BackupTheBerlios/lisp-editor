;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl)

(defpackage :gil-autodoc
  (:use :common-lisp :alexandria :generic :denest
	:package-stuff :path-stuff
	:gil :gil-share 
	:expression-scan)
  (:export 
   *mentionable-dependency* *path-root-mention* *path-root-link*
   mention-obj mention mention-path mention+ document
   def-document def-document-form)	  
  (:documentation "Produces GIL 'code' documentation.
Note: keyword 'way' arguments are the defaults."))

(in-package :gil-autodoc)

(defun serious-string (string)
  (if-let ((i (position #\Newline string)))
    (cons (subseq string 0 i) 
	  (cons :newline (serious-string (subseq string (+ i 1)))))
    (list string)))

(defun documentation* (of type)
  "Get documentatation string formatted for gil."
  (typecase-let (docstr (documentation of type))
    (null   nil)
    (string (glist-list :series (serious-string docstr)))
    (t      (warn "Documentation string ~s not a string!" docstr)
            "Doc string was not an object.")))

(defvar *also-internal* nil
  "Whether to also document the internal variables.")

(defun external-p (sym)
  "Laboriously checks if symbol is external.. Aught to be better way.
Probably will want document internal stuff too."
  (if (keywordp sym) t
    (when-let (pkg (symbol-package sym))
      (do-external-symbols (s pkg)
	(when (eql s sym) (return-from external-p t))))))

(defun documented-p (type name)
  "Whether something might be documented."
  (declare (ignore type))
  (external-p name))

(defun base-package (symbol)
  "Packages one might hit, but usually isn't interested in.\
 (trees in the forest.)"
  (find-if (rcurry #'same-package symbol) ;TODO SBCL specific.
	   '(:common-lisp :sb-impl :sb-int :sb-c :sb-pcl :sb-kernel)))

(defvar *mentionable-dependency* (lambda (sym)
				   (not (base-package sym)))
  "Function whether worth mentioning as dependency as based on symbol.")

;;Producing links to stuff.
(defgeneric give-name (mentionable)
  (:documentation "Assigns a name for the section of some mentionable\
 object. It is important to keep these constant! Otherwise, for html use,\
 links from outside will break."))

(defmethod give-name ((vector vector))
  (when (documented-p (aref vector 0) (aref vector 1))
    (gil-vars:gil-intern
     (give-name (access-result (aref vector 0) (aref vector 1))))))

(defmethod give-name ((null null)))

(defmethod give-name ((tf track-form))
  (gil-vars:gil-intern
   (destructuring-bind (type name &rest ignore)
       (slot-value tf 'expr-scan::form)
     (declare (ignore ignore))
     (if (eql type 'defpackage)
       (give-name (find-package name))
       (format nil "~a_~a_~a" (to-package-name name) type name)))))

(defmethod give-name ((fun track-fun))
  (gil-vars:gil-intern
   (with-slots (type name) fun
     (format nil "~a_~a_~a" (to-package-name name) type name))))

(defmethod give-name ((package package))
  (gil-vars:gil-intern (format nil "Package_~a" (package-name package))))

;;Mentioning stuff.
(defun mention-obj (mentioned objects &key (start 0))
  "Mention a scanned CL statement."
  (declare (type list objects))
  (glist-list
   (make-instance 'follow-link
     :name (when (or *also-internal* (external-p (name mentioned)))
	     (give-name mentioned)))
   (or objects (list (string-downcase (expr-scan:name mentioned)
				      :start start)))))

(defun mention (type name &rest objects)
  "Mention accessed object.(mention-obj if you already have the object."
  (mention-obj (access-result type name) 
	       (or objects (list (string-downcase name)))))

(defun mention+ (name &rest objects)
  "Mentions, assuming either (generic)function, macro, variable or\
 package (via keyword), in that order."
  (or
   (when-let (obj (access-result
		   '(defun defgeneric defmacro defvar defparameter)
		   name))
     (mention-obj obj objects))
   (when-let (pkg-obj (access-result 'defpackage name))
     (mention-obj pkg-obj objects :start 1))
   (unless (null objects)
     (glist-list :series objects))
   (u (string-downcase name))))

;;Mentioning files. (Files may get objects at some point?)
(defvar *path-root-mention* nil
  "Root of path for mentioning. (just filenames if nil)")
(defvar *path-root-link* nil
  "Root of path for linking. (no links if nil.")

(defun mention-path (path &rest objects)
  "Mentions a file."
  (flet ((mention-markup ()
	   (cond
	     (objects
	      (glist-list :series objects))
	     (*path-root-mention*
	      (from-path-root path *path-root-mention*))
	     (t
	      path))))
    (if *path-root-link*
      (url-link (from-path-root path *path-root-link*)
		(mention-markup))
      (mention-markup))))

;;Documentation.
(defgeneric document (manner object &key)
  (:documentation "Documentate things."))

(defmacro def-document (manner (object &rest keys) &body body)
  "Define (part of) a documenter of a object."
  (assert (eql (car keys) '&key) nil "Must start with a &key")
  (with-gensyms (man overrider)
    `(defmethod document
	 (,(if (keywordp manner) `(,man (eql ,manner)) manner) ,object
	  ,@keys)
       (if-let (,overrider(typecase ,(delist object)
			    ((or symbol string package list) nil)
			    (t (slot-value ,(delist object)
					   'expr-scan::overrider))))
	 (document ,(if (symbolp manner) manner (car manner)) ,overrider)
	 (progn ,@body)))))

(defmethod document (thing way &key)
  (declare (ignore thing way)))

(defgeneric document-form (manner name form)
  (:documentation "Distinguishes between diferrent track-forms."))

(defmacro def-document-form (manner name (&rest args) &body body)
  (with-gensyms (man nam form)
    `(progn
       (defmethod document (,(if (keywordp manner)
			       `(,man (eql ,manner)) manner)
			    (track-form track-form) &key)
	 (let ((form (slot-value track-form 'expr-scan::form)))
	   (document-form ,man (car form) form)))
       (defmethod document-form
	   (,(if (keywordp manner) `(,man (eql ,manner)) manner)
	    (,nam (eql ',name)) (,form list))
	 (destructuring-bind (,@args) (cdr ,form)
	   ,@body)))))

(defmethod document-form (manner (name symbol) form)
  nil)

;;Argument docs.
(defun document-args
    (args &key (allow-listing t)
     (special-variable-makers '(defvar defparameter) type))
  (flet ((arg (a)
	   (cond
	     ((and (listp a) (not type) allow-listing)
	      (document-args a :allow-listing allow-listing))
	     ((case a ((&optional &key &rest) t))
	      (setq type a) (series (string-downcase a) " "))
	     ((listp a)
	      (case type 
		(&rest
		 (error "Found a list in a &rest.\
 (Should be your fault, does CL notice it? ~a" args))
		((&key &optional)
		 (if-let (special-var
		   (when (symbolp (cadr a))
		     (access-result special-variable-makers (cadr a))))
		   (series
		    "(" (string-downcase (car a))
		    (mention-obj
		     special-var
		     (list (string-downcase
			    (expr-scan::name special-var))))
		    ")")))
		(t
		 (error "Sublisting not allowed, allow-listing=~D
Got ~D here" allow-listing a))))
	     (t
	      (series (string-downcase a) " ")))))
    (series "(" (glist-list :series (mapcar #'arg args) ")"))))

;;Dependency docs.
(defun package-sorted-symbol-list
    (list &key (types '(defun defgeneric defmacro))
               section-name pre-line)
  (denest
   (unless (null list))
   (let ((cur "") need-mention))
   (collecting (:onto deps)
     (denest
      (dolist (sym list))
      (when-let (pkg (to-package-name sym)))
      (cond
	((and (string= pkg cur) need-mention)
	 (collect ", " (mention types sym)))
	((not (string= pkg cur))
	 (setq cur pkg
	       need-mention (funcall *mentionable-dependency* sym))
	 (let ((mention (mention types sym)))
	   (when (and need-mention mention)
	     (collect
	      (series (unless (null deps) :newline)
		 (b "Package "
		    (mention 'defpackage (intern pkg :keyword)) ": ")
		 mention))))))))
   (return-from package-sorted-symbol-list
     (when deps
       (section 4 section-name pre-line
		(glist-list :series deps))))))

(defun document-dep (fun-dep var-dep
		     &key (want-fun t) (want-var t))
    ;TODO way to get round copy-list?
 (when (or fun-dep var-dep)
    (setq fun-dep (sort (copy-list fun-dep) #'symbol-package>) ;Sort them.
	  var-dep (sort (copy-list var-dep) #'symbol-package>))
    (series 
     (when (and fun-dep want-fun)
       (package-sorted-symbol-list
	fun-dep :types '(defun defgeneric defmacro) 
	:pre-line "Depends on functions:"))
     (when (and var-dep want-var)
       (package-sorted-symbol-list
	var-dep :types '(defvar defparameter)
	:pre-line "Depends on variables:")))))

(def-document :list-keywords ((track expr-scan:base-track) &key)
  "Treat keywords by just listing them."
  (series (format nil "Has keywords: ~{~a^, ~}"
		  (slot-value track 'expr-scan::keywords))))

(defvar *package-section-level* 1)

(defvar *full-level* 2)
(defvar *short-level* 4)
(defvar *short-link* nil "Whether to link to the short versions.")

(defun document-title
    (type name args &key with-args (allow-listing (not (eql type 'defun))))
  "Produces a title possibly with arguments."
  (series
   (string-downcase type) " " (string-downcase name) " "
   (when with-args
     (document-args args :allow-listing allow-listing))))

;;Anything docs.
(def-document :title-string ((thing base-track) &key) "")
(def-document :title-with-args ((thing base-track) &key)
  (document :title thing))
(def-document :args ((thing base-track) &key))
(def-document :description ((thing base-track) &key)
  (track-data :documentation thing))
(defmethod document :before ((manner (eql :dep)) (thing base-track) &key)
  (declare (ignore thing))) ;:before or it will steal the show.

(defvar *loop-block* nil
  "Blocks for loops that occur when user forgets to define something.")

(def-document :title-string (thing &key)
  (assert (not *loop-block*) nil
	  "Either define :title-string or :title or both for your object!")
  (let ((*lang* :txt) (*loop-block* t))
    (with-output-to-string (*standard-output*)
      (call (document :title thing)))))

(def-document :title (thing &key)
  (assert (not *loop-block*) nil
	  "Either define :title-string or :title or both for your object!")
  (let ((*loop-block* t))
    (document :title-string thing)))

(def-document :short ((thing base-track) &key)
  (section *short-level* (when *short-link* (give-name thing))
	   (document :title thing)
    (document :args thing)
    (document :description thing)))

(def-document :full ((thing base-track) &key)
  (section *full-level* (give-name thing) (document :title-with-args thing)
;    (p (document :args thing))
    (document :description thing)
    (p (document :dep thing))))

(def-document :dep ((dep depend-track) &key (want-fun t) (want-var t))
  (with-slots (fun-dep var-dep) dep
    (document-dep fun-dep var-dep :want-fun want-fun :want-var want-var)))

;;Document function.
(def-document :title ((fun track-fun) &key with-args)
  (with-slots (type name args) fun
    (document-title type name args :with-args with-args)))

(def-document :title-with-args ((fun track-fun) &key)
  (document :title fun :with-args t))

(def-document :description ((fun track-fun) &key)
  (or (track-data fun :documentation)
      (documentation* (name fun) 'function)))

(def-document :args ((fun track-fun) &key)
  (document-args (slot-value fun 'args)))

;(def-document :dep ((fun track-fun) &key (want-fun t) (want-var t))
;  "Unfortunately base-track steals the show."
;  (with-slots (fun-dep var-dep)
;      (document-dep 

;;Don't document methods (yet)
(def-document (anything t) ((method track-method) &key))

;;Document generic function.
(def-document :title ((fun track-generic) &key with-args)
  (destructuring-bind (name args &rest stuff)
      (cdr (slot-value fun 'expr-scan::form))
    (declare (ignore stuff))
    (document-title 'defgeneric name args :with-args with-args)))

(def-document :title-with-args ((fun track-generic) &key)
  (document :title fun :with-args t))

(def-document :description ((fun track-generic) &key)
  (or (track-data fun :documentation)
      (cadr(assoc :documentation
		  (cdddr (slot-value fun 'expr-scan::form))))
      (documentation* (cadr (slot-value fun 'expr-scan::form)) 'function)))

;;Document form.
(def-document :title-string ((track track-form) &key)
  (let ((form (slot-value track 'expr-scan::form)))
    (format nil "~a ~a"
	    (string-downcase (car form)) (string-downcase (cadr form)))))

;;Document variable.
(def-document :title-string ((var track-var) &key)
  (format nil "variable ~D"
	  (string-downcase (nth 1 (slot-value var 'expr-scan::form)))))

(def-document :description ((var track-var) &key)
  (or (track-data var :documentation)
      (nth 3 (slot-value var 'expr-scan::form))
      (documentation* (name var) 'variable)))

(defvar *by-names* '(defvar defparameter defun defmacro defgeneric)
  "Types of macros/functions to document. TODO allow names to 'pair up'")

;;Document package.(Note the mix of cl and expr-scan object.
(def-document :title-string ((package package) &key)
  (format nil "Package: ~a"
	  (string-downcase (package-name package) :start 1)))

(def-document-form :description defpackage (name &rest assoc)
  (or (track-data (access-result 'defpackage name) :documentation)
      (glist-list :series 
	(serious-string (cadr(assoc :documentation assoc))))
      (documentation* (to-package name) t)))

(def-document :file-origin ((track track-package) &key)
  "Documents the file origin of a package."
  (with-mod-slots "" (expr-scan::paths) track
    (when paths
      (series (format nil "Came from file~a: " (if (cdr paths) "s" ""))
	      (apply #'enumerate 
		     (mapcar (compose #'mention-path #'namestring)
			     paths))))))

(def-document :description ((package package) &key)
  (if-let (package-obj (access-result 'defpackage
			  (intern (package-name package) :keyword)))
    (document :description package-obj)
    (documentation* package t)))

(def-document :dep ((obj track-package) &key)
  (series "Uses packages: "
	  (apply #'enumerate (mapcar (curry #'mention 'defpackage)
				     (slot-value obj 'expr-scan::uses)))))

(def-document :also-dep ((obj track-package) &key)
  (series "Also uses packages: "
	  (apply #'enumerate
		 (mapcar (curry #'mention 'defpackage)
			 (slot-value obj 'expr-scan::also-uses)))))

(def-document way ((package package) &key)
  (when-let (package-obj (access-result 'defpackage
			   (intern (package-name package) :keyword)))
    (document way package-obj)))

;;Documenting lists of objects.
(def-document :descriptions ((object-list list) &key)
  "Makes a short list of just descriptions of objects."
  (glist-list :descriptions
    (mapcar (lambda (obj)
	      (list (document :title obj)
		    (document :description obj)))
	    object-list)))

(def-document (way symbol) ((object-list list) &key)
  "Other types of documenting a list of objects."
  (glist-list :series
    (mapcar (curry #'document way) object-list)))

;;Functions to handle lists of objects (from packages)
(defun sort-objects (list &optional (sort-compare :default))
  "Sorts objects."
  (sort list
	(case sort-compare
	  (:default
	   (lambda (a b)
	     (or (string> (expr-scan::etype a) (expr-scan::etype b))
		 (string> (expr-scan::name a) (expr-scan::name b)))))
	  (t
	   sort-compare))))

(defun get-objects-of-sym (sym &optional (by-names *by-names*) list)
  (dolist (by-name by-names list) ;Collect externals.
    (when-let (res (access-result by-name sym))
      (push res list))))

(defun get-external-objects (package &optional (sort-compare :default))
  "Gets sorted external objects of package."
  (let (list)
    (do-external-symbols (sym package)
      (when (same-package sym package)
	(gen:setf- append list (get-objects-of-sym sym))))
    (sort-objects list sort-compare)))

(defun get-internal-objects (package &optional (sort-compare :default))
  (let (list)
    (do-symbols (sym package)
      (when (and (same-package sym package) (not (external-p sym)))
	(gen:setf- append list (get-objects-of-sym sym))))
    (sort-objects list sort-compare)))

;;Documenting packages with contents.

(def-document :package-top
    ((package package) &key name (level* *package-section-level*) 
                            rest internal)
  (section level* name (glist (if name :series 
			        (make-instance 'follow-link
				  :name (give-name package)))
			      (when internal "Internal of") 
			      (document :title package))
    (document :description package)
    (p (document :dep package))
    (p (document :also-dep package))
    (p (document :file-origin package))
    (glist-list :series rest)))

(def-document :whole ((package package)
		      &key (sort-compare :default)
		      (also-internal *also-internal*)
		      (doc-manners '(:full)))
  (let*((name (give-name package))
	(i-name (format nil "~a=internal" name)))
    (flet ((doc (list &optional internal)
	     (document :package-top package
		       :name (if internal i-name name) :internal internal
	       :rest (cons
		      (when also-internal
			(if internal (p(b(link name "The external part.")))
			    (p (link i-name "The internal part."))))
		      (mapcar (lambda (manner)
				(document manner list))
			      doc-manners)))))
      (series
       (doc (get-external-objects package sort-compare))
       (when also-internal
	 (doc (get-internal-objects package sort-compare) t))))))

(def-document :pkg ((pkg-sym symbol)
		    &key (doc-manners '(:full)) also-internal)
  "Document package of the name. 
 (note that 'manner' here specifies what the object is whereas normally it\
 specifies what to do with the object.)"
  (document :whole (find-package pkg-sym)
    :doc-manners doc-manners :also-internal also-internal))

;;TODO if you figure out systems, document via them?

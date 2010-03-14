;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl)

(defpackage :gil-autodoc
  (:use :common-lisp :generic :denest :package-stuff
	:gil :gil-share 
	:expression-scan)
  (:export mention mention-package mention-obj document
	   *treat-title* *treat-args* *treat-args-in-title* *treat-dep*
	   *mentionable-dependency*)
  (:documentation "Produces GIL 'code' documentation.
Note: keyword 'way' arguments are the defaults.

TODO messy file."))

(in-package :gil-autodoc)

(defun documentation* (of type)
  (when-let docstr (documentation of type)
    docstr));TODO

(defvar *autodoc-dir* ""
  "Autodocumentation automatically to separate directory.
TODO implement")

(defvar *also-internal* nil
  "Whether to also document the internal variables.")

(defun external-p (sym)
  "Laboriously checks if symbol is external.. Aught to be better way.
Probably will want document internal stuff too."
  (if (keywordp sym) t
    (when-let pkg (symbol-package sym)
      (do-external-symbols (s pkg)
	(when (eql s sym) (return-from external-p t))))))

(defun maybe-documented-p (type name)
  "Whether something might be documented."
  (declare (ignore type))
  (external-p name))

(defun base-package (symbol)
  "Packages one might hit, but usually isn't interested in.\
 (trees in the forest.)"
  (find-if (curry #'same-package symbol) ;TODO SBCL specific.
	   '(:common-lisp :sb-impl :sb-int :sb-c :sb-pcl :sb-kernel)))
(defvar *mentionable-dependency* (lambda (sym)
				   (not (base-package sym)))
  "Function whether worth mentioning as dependency as based on symbol.")

(defgeneric give-name (mentionable)
  (:documentation "Assigns a name for the section of some mentionable\
 object. It is important to keep these constant! Otherwise, for html use,\
 links from outside will break."))

(defmethod give-name ((tf track-form))
  (destructuring-bind (type name &rest ignore)
      (slot-value tf 'expr-scan::form)
    (declare (ignore ignore))
    (if (eql type 'defpackage)
      (give-name (find-package name))
      (format nil "~a_~a_~a" (to-package-name name) type name))))

(defmethod give-name ((vector vector))
  (when (maybe-documented-p (aref vector 0) (aref vector 1))
    (give-name (access-result (aref vector 0) (aref vector 1)))))

(defmethod give-name ((null null))
  nil)

(defmethod give-name ((package package))
  (format nil "Package_~a" (package-name package)))

(defun mention-package (pkg &rest objects)
  "Separate mention function of mention of package."
  (glist-list (make-instance 'link :name (give-name (to-package pkg)))
    (or objects
	(list (string-downcase (to-package-name pkg) :start 1)))))

(defmethod give-name ((fun track-fun))
  (with-slots (type name) fun
    (format nil "~a_~a_~a" (to-package-name name) type name)))

(defun mention-obj (mentioned objects)
  "Mention a scanned CL statement."
  (declare (type list objects))
  (glist-list
   (if-let name (when (or *also-internal* (external-p (name mentioned)))
		  (give-name mentioned))
     (mk follow-link :name name) :underlined)
   (or objects (list (string-downcase (name mentioned))))))

(defgeneric i-mention (type name objects)
  (:documentation "Mentions an object. Might fiddle with arguments in case\
 it is ambiguous."))

(defmethod i-mention (type name objects)
  (mention-obj (access-result type name)
	       (or objects (list (string-downcase name)))))

(defmethod i-mention ((type (eql 'defpackage)) pkg-name objects)
  (mention-obj
   (access-result 'defpackage
		  (when-let pkg (find-package pkg-name)
		    (intern (package-name pkg) :keyword)))
   (or objects (list (string-downcase pkg-name :start 1)))))

(defmethod i-mention ((type (eql 'variable)) name objects)
  (mention-obj (access-result '(defvar defparameter) name) objects))

(defmethod i-mention ((type (eql 'function)) name objects)
  (mention-obj (access-result '(defgeneric defun defmacro) name) objects))

(defun mention (type name &rest objects)
  "Mention accessed object.(mention-obj if you already have the object."
  (when (maybe-documented-p type name)
    (i-mention type name objects)))
  
(defgeneric document (manner object &key)
  (:documentation "Documentate things."))

(defmacro def-document (manner (object &rest keys) &body body)
  "Define (part of) a documenter of a object."
  (assert (eql (car keys) '&key) nil "Must start with a &key")
  (with-gensyms (man)
    `(defmethod document
	 (,(if (keywordp manner) `(,man (eql ,manner)) manner) ,object
	  ,@keys)
       ,@body)))

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

(defvar *treat-args-in-title* :args)

;;Argument docs.
(defun document-args
    (args &key (allow-listing t)
     (special-variable-makers '(defvar defparameter) type))
  (collecting (nil arg-doc)
    (dolist (a args)
      (collecting
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
	     (if-let special-var
	       (when (symbolp (cadr a))
		 (access-result special-variable-makers (cadr a)))
	       (series
		"(" (string-downcase (car a))
		(mention-obj
		 special-var
		 (list (string-downcase (expr-scan::name special-var))))
		")")))
	    (t
	     (error "Sublisting not allowed, allow-listing=~D
Got ~D here" allow-listing a))))
	 (t
	  (series (string-downcase a) " ")))))
    (return-from document-args
      (series "(" (glist-list :series arg-doc) ")"))))

;;Dependency docs.
(defun package-sorted-symbol-list
    (list &key (types '(defun defgeneric defmacro))
               section-name pre-line)
  (denest
   (unless (null list))
   (let ((cur "") need-mention))
   (collecting (nil deps)
     (dolist (sym list)
       (let ((pkg (to-package-name sym)))
	 (cond
	   ((and (string= pkg cur) need-mention)
	    (collecting ", " (mention types sym)))
	   ((not (string= pkg cur))
	    (setq cur pkg
		  need-mention (funcall *mentionable-dependency* sym))
	    (let ((mention (mention types sym)))
	      (when (and need-mention mention)
		(collecting
		 (series (unless (null deps) :newline)
		   (b "Package "
		      (mention 'defpackage (intern pkg :keyword)) ": ")
		   mention)))))))))		
     (return-from package-sorted-symbol-list
       (when deps
	 (section 4 section-name pre-line
		  (glist-list :series deps))))))

(defun document-dep (fun-dep var-dep
		     &key (want-fun-dep t) (want-var-dep t))
    ;TODO way to get round copy-list?
  (when (or fun-dep var-dep)
    (setq fun-dep (sort (copy-list fun-dep) #'symbol-package>) ;Sort them.
	  var-dep (sort (copy-list var-dep) #'symbol-package>))
    (series 
     (when (and fun-dep want-fun-dep)
       (package-sorted-symbol-list
	fun-dep :types '(defun defgeneric defmacro) 
	:pre-line "Depends on functions:"))
     (when (and var-dep want-var-dep)
       (package-sorted-symbol-list
	var-dep :types '(defvar defparameter)
	:pre-line "Depends on variables:")))))

(def-document :list-keywords ((track expr-scan:base-track) &key)
  "Treat keywords by just listing them."
  (series (format nil "Has keywords: ~{~a^, ~}"
		  (slot-value track 'expr-scan::keywords))))

(defvar *treat-title* :title "Manner to treat title.")
(defvar *treat-args* :args "Manner to treat arguments.")
(defvar *treat-dep* :dep "Manner to treat dependencies.")

(defvar *package-section-level* 1)

(defvar *full-level* 2)
(defvar *short-level* 4)
(defvar *short-link* nil "Whether to link to the short versions.")

(defun document-title
    (type name args &key with-args (allow-listing (not (eql type 'defun))))
  "Produces a title possibly with arguments."
  (series
   (string-downcase type) " " (string-downcase name)
   (when (or with-args *treat-args-in-title*)
     (document-args args :allow-listing allow-listing))))

;;Anything docs.
(def-document :title ((thing base-track) &key))
(def-document :title-with-args ((thing base-track) &key)
  (document :title thing))
(def-document :args ((thing base-track) &key))
(def-document :description ((thing base-track) &key)
  (track-data :documentation thing))
(def-document :dep ((thing base-track) &key))

(def-document :short ((thing base-track) &key)
  (section *short-level* (when *short-link* (give-name thing))
	   (document :title thing)
    (when *treat-args* (document :args thing))
    (document :description thing)))

(def-document :full ((thing base-track) &key)
  (section *full-level* (give-name thing) (document :title thing)
    (when *treat-args* (p (document :args thing)))
    (document :description thing)
    (when *treat-dep*
      (p (document :dep thing)))))

(def-document :dep ((dep depend-track) &key want-fun want-var)
  (with-slots (fun-dep var-dep) dep
    (document-dep fun-dep var-dep :want-fun want-fun :want-var want-var)))

;;Function docs.
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

;;Generic docs.
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

;;Document variable.
(def-document :title ((var track-var) &key)
  (format nil "variable ~D"
	  (string-downcase (nth 1 (slot-value var 'expr-scan::form)))))

(def-document :description ((var track-var) &key)
  (or (track-data var :documentation)
      (nth 3 (slot-value var 'expr-scan::form))
      (documentation* (name var) 'variable)))

(defvar *by-names* '(defvar defparameter defun defmacro defgeneric)
  "Types of macros/functions to document. TODO allow names to 'pair up'")

;;Document package.(Note the mix of cl and expr-scan object.
(def-document :title ((package package) &key)
  (format nil "Package: ~a"
	  (string-downcase (package-name package) :start 1)))

(def-document-form :description defpackage (name &rest assoc)
  (or (track-data (access-result 'defpackage name) :documentation)
      (cadr(assoc :documentation assoc))
      (documentation* (to-package name) t)))

(def-document :description ((package package) &key)
  (if-let package-obj (access-result 'defpackage
			  (intern (package-name package) :keyword))
    (document :description package-obj)
    (documentation* package t)))

(def-document-form :dep defpackage (name &rest assoc)
  (declare (ignore name))
  (let ((uses (cdr(assoc :use assoc))))
    (series "Uses packages: " (mention 'defpackage (car uses))
       (glist-list :series
	 (mapcan (lambda (pkg)
		   (list ", " (mention 'defpackage pkg)))
		 (cdr uses))))))

(def-document :dep ((package package) &key)
  (when-let package-obj (access-result 'defpackage
			  (intern (package-name package) :keyword))
    (document :dep package-obj)))

;;Documenting lists of objects.
(def-document :descriptions ((object-list list) &key)
  "Makes a short list of just descriptions of objects."
  (glist-list :descriptions
    (mapcar (lambda (obj)
	      (list (let ((*treat-args-in-title* nil))
		      (document :title obj))
		    (document :description obj)))
	    object-list)))

(def-document (way symbol) ((object-list list) &key)
  "Other types of documenting a list of objects."
  (glist-list :series
    (mapcar (lambda (obj) (document way obj)) object-list)))

;;Functions to handle lists of objects (from packages)
(defun sort-objects (list &optional (sort-compare :alphabetic))
  "Sorts objects."
  (sort list
	(case sort-compare
	  (:alphabetic
	   (lambda (a b)
	     (string> (expr-scan::name a) (expr-scan::name b))))
	  (t
	   sort-compare))))

(defun get-objects-of-sym (sym &optional (by-names *by-names*) list)
  (dolist (by-name by-names list) ;Collect externals.
    (when-let res (access-result by-name sym)
      (push res list))))

(defun get-external-objects (package &optional (sort-compare :alphabetic))
  "Gets sorted external objects of package."
  (let (list)
    (do-external-symbols (sym package)
      (when (same-package sym package)
	(setf- append list (get-objects-of-sym sym))))
    (sort-objects list sort-compare)))

(defun get-internal-objects (package &optional (sort-compare :alphabetic))
  (let (list)
    (do-symbols (sym package)
      (when (and (same-package sym package) (not (external-p sym)))
	(setf- append list (get-objects-of-sym sym))))
    (sort-objects list sort-compare)))

;;Documenting packages with contents.

(def-document :whole ((package package)
		      &key (sort-compare :alphabetic)
		      with-descriptions with-shorts (with-full t)
		      (also-internal *also-internal*))
  (let*((name (give-name package))
	(i-name (format nil "~a=internal" name)))
    (flet ((doc (list &optional internal)
	     (section *package-section-level* (if internal i-name name)
		      (series (when internal "Internal of ")
			      (document :title package))
	       (document :description package)
	       (when also-internal
		 (if internal (p(b(link name "The external part.")))
		     (p (link i-name "The internal part."))))
	       (when *treat-dep*
		 (p (document :dep package)))
	       (when with-descriptions (document :descriptions list))
	       (when with-shorts (document :short list))
	       (when with-full   (document :full list)))))
      (series
       (doc (get-external-objects package sort-compare))
       (when also-internal
	 (doc (get-internal-objects package sort-compare) t))))))

(def-document :pkg ((pkg-sym symbol) &key
		    with-descriptions with-shorts (with-full t)
		    also-internal)
  "Document package of the name. 
 (note that 'manner' here specifies what the object is whereas normally it\
 specifies what to do with the object.)"
  (document :whole (find-package pkg-sym)
    :with-descriptions with-descriptions :with-shorts with-shorts
    :with-full with-full :also-internal also-internal))

;;TODO if you figure out systems, document via them?

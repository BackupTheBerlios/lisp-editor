;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl)

(defpackage :autodoc-gil
  (:use :common-lisp :generic :denest :package-stuff
	:gil :gil-share 
	:expression-scan)
  (:export mention mention-obj document
	   *treat-title* *treat-args* *treat-args-in-title* *treat-dep*
	   *mentionable-dependency*)
  (:documentation "Produces GIL 'code' documentation.
Note: keyword 'way' arguments are the defaults."))

(in-package :autodoc-gil)

(defvar *autodoc-dir* ""
  "Autodocumentation automatically to separate directory.
TODO implement")

(defvar *scan-doc-string* nil
  "Whether to scan doc strings as GIL.")

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
    (format nil "~D_~D" type name)))

(defmethod give-name ((vector vector))
  (when (maybe-documented-p (aref vector 0) (aref vector 1))
    (give-name (access-result (aref vector 0) (aref vector 1)))))

(defmethod give-name ((null null))
  nil)

(defmethod give-name ((fun track-fun))
  (with-slots (type name) fun
    (format nil "~D ~D" type name)))

(defun object-section (level of-object title &rest objects)
  (apply #'section `(,level ,(give-name of-object) ,title ,@objects)))

(defun mention-obj (mentioned objects)
  "Mention a scanned CL statement."
  (assert (listp objects) nil
	  "Mention-obj takes a _list_ of objects as arguments.")
  (glist-list
   (if-let name (when (external-p (name mentioned)) (give-name mentioned))
     (mk follow-link :name name) :underlined)
   (or objects (list (string-downcase (name mentioned))))))

(defgeneric i-mention (type name objects)
  (:documentation "Mentions an object. Might fiddle with arguments in case\
 it is ambiguous."))

(defmethod i-mention (type name objects)
  (mention-obj (access-result type name)
	       (if-use objects (list (string-downcase name)))))

(defmethod i-mention ((type (eql 'defpackage)) pkg-name objects)
  (mention-obj
   (access-result 'defpackage
		  (when-let pkg (find-package pkg-name)
		    (intern (package-name pkg) :keyword)))
   (if-use objects (list (string-downcase pkg-name :start 1)))))

(defun raw-mention-var (name objects)
  "Clump various types of variables together."
  (mention-obj (obj (access-result 'defvar name)
		    (access-result 'defparameter name) 
		    (access-result 'variable name))
	       objects))

(defmethod i-mention ((type (eql 'variable)) name objects)
  (raw-mention-var name objects))
(defmethod i-mention ((type (eql 'variable)) name objects)
  (raw-mention-var name objects))
(defmethod i-mention ((type (eql 'variable)) name objects)
  (raw-mention-var name objects))

(defun mention (type name &rest objects)
  "Mention accessed object.(mention-obj if you already have the object."
  (when (maybe-documented-p type name)
    (i-mention type name objects)))
  
(defgeneric document (thing manner &key)
  (:documentation "Documentate things."))  

(defmethod document (thing manner &key)
  (p "Don't know how to document ~D in manner ~D."
     (type-of thing) manner))

(defgeneric document-form (name form manner)
  (:documentation "Distinguishes between diferrent track-forms."))
(defmethod document ((track-form track-form) manner &key)
  (let ((form (slot-value track-form 'expr-scan::form)))
    (document-form (car form) form manner)))
(defmethod document-form ((name symbol) form manner)
  nil)

(defun sym-name (sym)
  (string-downcase (symbol-name sym)))

(defvar *treat-args-in-title* :args)

;;Title.
(defmethod document ((info list) (way (eql :title)) &key with-args)
  "Produces a title of a track-fun."
  (destructuring-bind (type name args) info
    (series
     (sym-name type) " " (sym-name name)
     (when (or with-args *treat-args-in-title*)
       (document args *treat-args-in-title*
		 :allow-listing (not(eql type 'defun)))))))

;;Argument docs.
(defmethod document ((args list) (way (eql :args))
		     &key allow-listing type
		     (special-variable-makers '(defvar defparameter)))
  (collecting (nil arg-doc)
    (dolist (a args)
      (collecting
       (cond
	 ((and (listp a) (not type) allow-listing)
	  (document a way :allow-listing allow-listing))
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
		"(" (sym-name (car a))
		(mention-obj
		 special-var
		 (list (sym-name (expr-scan::name special-var))))
		")")))
	    (t
	     (error "Sublisting not allowed, allow-listing=~D
Got ~D here" allow-listing a))))
	 (t
	  (series (sym-name a) " ")))))
    (return-from document
      (series "(" (glist-list :series arg-doc) ")"))))

;Title with arguments currently the same.
(defmethod document ((info list) (way (eql :title-args)) &key)
  (document info :title :with-args t))

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

(defmethod document ((info list) (way (eql :dep))
		     &key (want-fun-dep t) (want-var-dep t))
  (destructuring-bind (name fun-dep var-dep) info
    (declare (ignore name))
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
	  :pre-line "Depends on variables:"))))))

(defvar *treat-title* :title "Manner to treat title.")
(defvar *treat-args* :args "Manner to treat arguments.")
(defvar *treat-dep* :dep "Manner to treat dependencies.")

(defmethod document (thing way &key level)
  (lambda ()))

;;Function docs.
(defmethod document
    ((fun track-fun) (level integer)
     &key (tp 'function)
     (allow-listing (not (eql (slot-value fun 'expr-scan::type) 'defun))))
;TODO defaultly make arrangement of arguments depend on length.
  (with-slots (type name args fun-dep var-dep) fun
    (object-section level fun
	     (document (list type name args) *treat-title* 
		       :allow-listing allow-listing)
      (when *treat-args*
	(p (document args *treat-args* :allow-listing allow-listing)))
      (p (let ((doc-string (documentation name tp)))
	   (if *scan-doc-string*
	     (with-input-from-string (stream doc-string)
	       (glist-list :series 
			   (gil-read::gil-read-col stream)))
	     doc-string)))
      (when *treat-dep*
	(p (document (list name fun-dep var-dep) *treat-dep*))))))

;;Generic docs.
 ;TODO currently very basic, info in particular methods appreciated.
(defmethod document ((fun track-generic) (level integer) &key)
  (let ((title (format nil "defgeneric ~D"
		       (string-downcase (expr-scan::name fun)))))
    (object-section level fun title
      (cadr(assoc :documentation
		  (cdddr (slot-value fun 'expr-scan::form)))))))

;Document variable.
(defmethod document ((var track-var) (level integer) &key (tp 'variable))
  (with-slots (expr-scan::form fun-dep var-dep) var
    (destructuring-bind (type name &optional init doc) expr-scan::form
      (declare (ignore init typr)) ;TODO don't ignore?
      (object-section level var
	       (format nil "variable ~D" (string-downcase name))
        doc))))

(defvar *by-names* '(defvar defparameter defun defmacro defgeneric)
  "Types of macros/functions to document. TODO allow names to 'pair up'")

(defvar *section-level* 1)

(defmethod document-form
    ((name (eql 'defpackage)) form (manner (eql :title-less)))
  (series
   (p (cadr(assoc :documentation (cddr form))))
   (let ((uses (cdr(assoc :use (cddr form)))))
     (p "Uses packages: " (mention 'defpackage (car uses))
	(glist-list :series
	  (mapcan (lambda (pkg)
		    (list ", " (mention 'defpackage pkg)))
		  (cdr uses)))))))

;;Document package. Note: doesn't scan it for you.
(defmethod document
    ((package package) manner
     &key (level 2)
     (package-name (package-name package))
     (package-obj
      (access-result 'defpackage (intern package-name :keyword)))
     (section-name
      (if package-obj
        (give-name package-obj)
	(format nil "package-~a" package-name)))
     (section-title
      (format nil "Package: ~a~a"
	 (subseq package-name 0 1) 
	 (string-downcase (subseq package-name 1))))
     (divider nil))
  (let (res-list)
    (do-external-symbols (sym package)
      (dolist (by-name *by-names*) ;Collect externals.
	(when-let res (when (same-package sym package)
			(access-result by-name sym))
	  (push res (getf res-list by-name))
	  (return))))
    (let*((ordered-externals ;Put externals in order.
	   (mapcan 
	     (lambda (by-name)
	       (when-let got (getf res-list by-name)
		 (setf- sort got
			(lambda (a b)
			  (string> 
			   (symbol-name (expr-scan::name a))
			   (symbol-name (expr-scan::name b)))))
		 (if divider (cons divider got) got)))
	     *by-names*))
	  (total
	   (series
	    (if ordered-externals
	      (glist-list :series 
	        (mapcar (lambda (res) ;document the parts.
			  (document res level))
		       ordered-externals))
	      (b "Nothing exported."))
	    :hr)))
      (if *section-level*
	(section *section-level* section-name section-title
          (if package-obj (document package-obj :title-less)
	                  (documentation package t))
	  total)
	total))))

(defmethod document ((pkg-sym symbol) (manner (eql :pkg))
		     &key (level 1))
  (document (find-package pkg-sym) t :level level))

;;Document system. (Needs to be scanned, will defaultly scan for you.)
#|(defmethod document
    ((sys asdf:system) way &key (level 1) (pre-scan t))
  (when pre-scan
    (scan-system sys))
  (let ((pkgs
	 (car (getf expr-scan::*system-packages*
		    (intern (asdf:component-name sys))))))
    (glist-list :series 
		(mapcar (lambda (pkg)
			  (document pkg t :level level ))
			pkgs))))|#

(defmethod document
    ((system-symbol symbol) (way (eql :sys)) &key (level 1) (pre-scan t))
  (document (asdf:find-system system-symbol) t
	    :level level :pre-scan pre-scan))

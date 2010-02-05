
(cl:in-package :cl)

(defpackage :autodoc-gil
  (:use :common-lisp :generic :denest :package-stuff
	:gil :gil-share 
	:expression-scan)
  (:export document)
  (:documentation "Produces GIL 'code' documentation.
Note: keyword 'way' arguments are the defaults."))

;;TODO all those 'not a known argument keyword' style warnings.
;;TODO do some stuff with hooks that are done with variables now?

;TODO documenting defpackage, defgeneric, defmethod, defclass, defstruct.
(in-package :autodoc-gil)

(defvar *scan-doc-string* nil
  "Whether to scan doc strings as GIL.")

(defun base-package (symbol)
  "Packages one might hit, but usually isn't interested in.\
 (trees in the forest.)"
  (find-if (curry #'same-package symbol) ;TODO SBCL specific.
	   '(:common-lisp :sb-impl :sb-int :sb-c :sb-pcl :sb-kernel)))

(defvar *mentionable-dependency* (lambda (sym within)
				   (declare (ignore within))
				   (not (base-package sym)))
  "Whether worth mentioning as dependency.")

(defgeneric give-name (mentionable))

(defmethod give-name ((tf expr-scan::track-form))
  (destructuring-bind (type name &rest ignore) (slot-value tf 'format)
    (declare (ignore ignore))
    (format nil "~D ~D" type name)))

(defgeneric mention (mentioned object)
  (:documentation "Mention an object."))

(defmethod mention (mentionable object)
  (link object (give-name mentionable)))

(defgeneric document (thing manner &key)
  (:documentation "Documentating things."))  

(defmethod document (thing manner &key)
  (p "Don't know how to document ~D in manner ~D ."
     (type-of thing) manner))

(defun sym-name (sym)
  (string-downcase (symbol-name sym)))

(defvar *treat-args-in-title* nil)

;;Title.
(defmethod document ((info list) (way (eql :title)) &key with-args)
  "Produces a title of a track-fun."
  (destructuring-bind (type name args) info
    (glist :series
      (sym-name type) " " (sym-name name)
      (when (or with-args *treat-args-in-title*)
	(document args *treat-args-in-title*)))))

(defmethod document
    ((info list) (way (eql :title-args)) &key)
  (document info :title))

;;Argument docs.
(defmethod document ((args list) (way (eql :args))
		     &key allow-listing
		     (special-variable-makers '(defvar defparameter)))
  (apply #'glist
    `(:series "("
      ,@(let (type)
	  (mapcar
	   (lambda (a)
	     (cond
	       ((and (listp a) (not type) allow-listing)
		(document a way :allow-listing allow-listing))
	       ((case a ((&optional &key &rest) t))
		(setq type a) nil)
	       ((listp a)
		(case type 
		  (&rest
		   (error "Found a list in a &rest.\
 (Should be your fault, does CL notice it?"))
		  ((&key &optional)
		   (if-let special-var
			   (when (symbolp (cadr a))
			     (access-result special-variable-makers (cadr a)))
		     (glist :series
		       "(" (sym-name (car a))
		       (mention
			special-var
			(sym-name (expr-scan::name special-var)))
		       ")")))
		  (t
		   (error "Sublisting not allowed, allow-listing=~D
Got ~D here" allow-listing a))))
	       (t
		(glist :series (sym-name a) " "))))
	   args))
      ")")))

;Title with arguments currently the same.
(defmethod document ((args list) (way (eql :title-args)) &key)
  (document args :args))

;;Dependency docs.
(defun package-sorted-symbol-list (list &key name within-package)
  (let ((cur "") need-mention)
    (apply #'section
      `(4 ,name
	"Depends on functions:"
	,@(mapcan
	   (lambda (sym &key (pkg (to-package-name sym)))
	     (cond
	       ((and (string= pkg cur) need-mention)
		(list ", " (sym-name sym)))
	       ((not (string= pkg cur))
		(setq cur pkg
		      need-mention (funcall *mentionable-dependency* 
					    sym within-package))
		(when need-mention
		  (list 
		   (header
		    5 (glist :series "Package " (string-downcase pkg)))
		   (sym-name sym))))))
	   list)))))

(defmethod document ((info list) (way (eql :dep))
		     &key (want-fun-dep t) (want-var-dep t))
  (destructuring-bind (name fun-dep var-dep) info
    ;TODO way to get round copy-list?
    (setq fun-dep (sort (copy-list fun-dep) #'symbol-package>) ;Sort them.
	  var-dep (sort (copy-list var-dep) #'symbol-package>))
    (apply #'glist
      `(:series
	,(when (and fun-dep want-fun-dep)
	    (package-sorted-symbol-list
	     fun-dep :name (format nil "funs-of-~D" name)))
	,(when (and var-dep want-var-dep)
	    (package-sorted-symbol-list
	     var-dep :name (format nil "vars-of-~D" name)))))))

(defmethod give-name ((fun track-fun))
  (with-slots (type name) fun
    (format nil "~D ~D" type name)))

(defvar *treat-title* :title "Manner to treat title.")
(defvar *treat-args* :args "Manner to treat arguments.")
(defvar *treat-dep* :dep "Manner to treat dependencies.")

(defmethod document (thing way &key)
  (lambda ()))

;;Function docs.
(defmethod document
    ((fun track-fun) (level integer)
     &key (tp 'function)
     (allow-listing (not (eql (slot-value fun 'expr-scan::type) 'defun))))
;TODO defaultly make arrangement of arguments depend on length.
  (with-slots (type name args fun-dep var-dep) fun
    (section level (give-name fun)
	     (document (list type name args) *treat-title*)
      (when *treat-args*
	(p (document args *treat-args* :allow-listing allow-listing)))
      (p (let ((doc-string (documentation name tp)))
	   (if *scan-doc-string*
	     (with-input-from-string (stream doc-string)
	       (eval `(*lang* ,*lang* ;WARNING eval is a biter!
			(glist :series 
			       ,@(gil-read:gil-read doc-str)))))
	     doc-string)))
      (when *treat-dep*
	(p (document (list name fun-dep var-dep) *treat-dep*))))))

;;Generic docs.
 ;TODO currently very basic, info in particular methods appreciated.
(defmethod document ((fun track-generic) (level integer) &key)
  (let ((title (format nil "defgeneric ~D"
		       (string-downcase (expr-scan::name fun)))))
    (section level title title
      (cadr(assoc :documentation (cdddr (slot-value fun 'expr-scan::form)))))))

;Document variable.
(defmethod document ((var track-var) (level integer) &key (tp 'variable))
  (with-slots (expr-scan::form fun-dep var-dep) var
    (destructuring-bind (type name &optional init doc) expr-scan::form
      (declare (ignore init typr)) ;TODO don't ignore?
      (section level (symbol-name (expr-scan::name var))
	       (format nil "variable ~D" (string-downcase name))
        doc))))

(defvar *by-names* '(defvar defparameter defun defmacro defgeneric)
  "Types of macros/functions to document. TODO allow names to 'pair up'")

;;Document package. Note: doesn't scan it for you.
(defmethod document
    ((package package) manner
     &key (level 2)
     (section-level 1) ;TODO sense many variables again.
     (section-name
      (let ((name (package-name package)))
	(concatenate 'string
	  (subseq name 0 1) (string-downcase (subseq name 1)))))
     (section-title section-name)
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
			   (symbol-name (expr-scan::name a)))))
		 (if divider (cons divider got) got)))
	     *by-names*))
	  (total
	   (mapcar 
	    (lambda (res) ;document the parts.
	      (document res level))
	    ordered-externals)))
      (if section-level
	(apply #'section
	       `(,section-level ,section-name ,section-title ,@total))
	(apply #'glist
	       `(:series ,@total))))))

(defmethod document ((pkg-sym symbol) (manner (eql :pkg))
		     &key (level 1))
  (document (find-package pkg-sym) t :level level))

;;Document system. (Needs to be scanned, will defaultly scan for you.)
(defmethod document
    ((sys asdf:system) way &key (level 1) (pre-scan t))
  (when pre-scan
    (scan-system sys))
  (let ((pkgs
	 (car (getf expr-scan::*system-packages*
		    (intern (asdf:component-name sys))))))
    (apply #'glist
	   (cons :series
		 (mapcar (lambda (pkg)
			   (document pkg t :level level ))
			 pkgs)))))

(defmethod document
    ((system-symbol symbol) (way (eql :sys)) &key (level 1))
  (document (asdf:find-system system-symbol) t :level level))

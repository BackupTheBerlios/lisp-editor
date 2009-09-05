(cl:in-package :cl)

(defpackage :autodoc
  (:use :common-lisp :generic :denest
	:package-stuff :lml2
	:expression-scan)
  )

(in-package :autodoc)

(defun argument-doc-string (type name argument-name)
  "Get documentation string of an argument. Setfable.

When argument-name is a keyword, it can refer to orther things,
 like :long-doc-str"
  (getf (getf (slot-value (access-result type name) 'other) :arg-docs)
	argument-name))
(defun (setf argument-doc-string) (to type name argument-name)
  "See non-setf version."
  (setf (getf (getf (slot-value (access-result type name) 'other) :arg-docs)
	      argument-name) to))

(defun documentation-set (type name args-docs &key delete-prior)
  "Make a complete set of documentation. (delete-prior makes this the only\
 documentation."
  (when delete-prior
    (setf (getf (slot-value (gethash (vector type name) *scan-result)
			    'other) :arg-docs) nil))
  (when obj-doc
    (setf (documentation name (case type
				((defmacro defun) 'function)
				((defvar defparameter) 'variable)))
	  obj-doc))
  (do ((i args-doc (cddr i)))
      ((null i) nil)
    (setf (argument-doc-string type name (car i)) (cadr i))))

(defvar *document-prepend*
(defvar *document-long-version* nil)
(defvar *document-link-packages* nil)

(defvar *document-file* "")

(defgeneric header (type))

(defmacro header-of (type header)
  `(defmethod header ((tp (eql ,type)))
     ,header))
(header-of :object :h2)
(header-of :args :h3)
(header-of :arg  :h4)
(header-of :links :h3)
(header-of :link-package :h4)

(defgeneric autodoc (thing way)
  (:documentation "Automatically document a thing."))

(defgeneric mention (thing way)
  )

(defmethod mention ((vec vector) (way :a-lml2)) ;Mention firstly. (once!)
  (let ((type (aref vec 0)) (name (aref vec 1)))
    `((:a :name ,(format nil "~D_~D" type name))
      ,(format nil "~D ~D" type name))))

(defmethod mention ((vec vector) (way :lml2)) ;Mention referring.
  (let ((type (aref vec 0)) (name (aref vec 1)))
    `((:a :href ,(format nil "~D#~D_~D" *document-file* type name))
      ,(format nil "~D" name))))

(defmethod mention ((tfn track-fun) way)
  (with-slots (type name other) tfn
    (let ((*document-file* (if-use (getf other :doc-file) *document-file*)))
      (mention (vector type name) way))))

(defun mention-fun-symbol (symbol way)
  "Mention the function/macro attached to symbol."
  (mention (if-use (access-result 'defun symbol)
		   (access-result 'defmacro symbol)) way))

(defun arguments-text (args &key mode)
  "Produces a little text representing the arguments."
  (denest (collecting ())
	  (dolist (a args)
	    (cond ((listp a)
		   (case mode
		     ((&key &optional)
		      (collecting (format nil "~S" a)))
		     (t
		      (appending `("(" ,@(arguments-text a) ")")))))
		  ((symbolp a)
		   (case a
		     ((&key &optional &rest &body)
		      (setf mode a)))
		   (collecting (symbol-name a))))
	    (collecting " "))))

(defun map-arguments (args fun &key mode)
  "Map over the arguments in reading-order."
  (dolist (a args)
    (case a
      ((&key &optional &rest) (setf mode a))
      (t
       (case mode
	 ((&key &optional &rest)
	  (funcall fun a mode))
	 (t
	  (if (listp a) (map-arguments a fun) (funcall fun a mode))))))))

(defun document-arguments (args arg-docs)
  "Document on the argument-special doc-strings."
  (when arg-docs 
    `((,(header :args) "Arguments")
      ,@(denest ;soo nested :/
	 (collecting ())
	 (map-arguments args)
	 (lambda (a mode))
	 (appending)
	 `((,(header :arg)
	     ,(case mode
		(&key "Keyword ")
		(&optional "Optional argument ")
		(&rest "Rest argument."))
	     (:p ,(getf arg-docs (if (listp a) (car a))))
	     ,@(when (listp a)
		 `((:p ,(format nil "Defaultly: ~A" (cadr a)))))))))))

(defun document-links (dep self &key (way :lml2))
  "Document useage of function/macros in other functions/macros."
  (setf dep (sort dep #'symbol-package>)) ;Sort by package.
  (denest
   ((collecting (nil own-pkg col-own))
    (collecting (nil rest col-rest))
   ;Need to alter them to only accept the wanted ones, and to mention them.
    (flet ((col-own (collect)
	     (when (listfind-package collect *document-link-packages*)
	       (col-own (mention-fun-symbol collect way))))
	   (col-rest (collect)
	     (when (listfind-package collect *document-link-packages*)
	       (col-rest (mention-fun-symbol collect way)))))
    (let (prev)) ;Store previous.
    (:return ;How to return after.
      `((,(header :links) "Used macros and functions")
	(,(header :link-package) "From package itself")
	,@(if (null own-pkg) '("None.") own-pkg)
	,@rest))
    (dolist (d dep)) ;Iterate.
    (cond
      ((same-package self d)
       (col-own d))
      ((same-package d prev)
       (col-rest d))
      (t
       (setf prev d)
       (col-rest `(,(header :link-package) "From package "
		    ,(format nil "~A" (to-package-name d))))
       (col-rest d))))))

(defmethod autodoc ((tfn track-fun) (way :lml2))
  (with-slots (type name args dep #|var-dep|# other) tfn
    (setf
     (getf other :lml2) ;Name, type.
     `((,(header :object) ,(mention tfn :a-lml2))
	 ,@(arguments-text args)) ;Arguments.
        ;(TODO whether arguments at head based on length.)
      ;Documentation string.
       ,(documentation name 'function) 
      ;Documentation of the arguments.
       ,@(document-arguments args (getf other :arg-docs))
      ;Documentation of links to other functions.
       ,@(when *document-link-packages*
	  (document-links dep self))))))

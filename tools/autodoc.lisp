(cl:in-package :cl-user)

;TODO it is a little messy. Probably better make the autodoc separate 
; functions?

(defpackage :autodoc
  (:use :common-lisp :generic :plist-slot :denest
	:package-stuff :lml2 :mk-website
	:expression-scan :example)
  (:export autodoc document-pages document-pages-write
	   interactive-doc
	   *document-file* document-system)
  (:documentation "Tools to autodocument things based on CL data,\
 information gathered with expression-scan, and extra information can be\
 added.
If there is asdf, can be used like\
 (asdf:operate 'autodoc:document system)

TODO linking between different files seems entirely broken."))

;TODO better package documentation.
(in-package :autodoc)

;TODO these in separate parts?
(defun clear-doc (type name pslot)
  "Clear a piece of extra documentation."
  (setf (pslot (access-result type name) pslot) nil))

(gen:setf-defun doc-argument (type name arg-name)
  "Get/set documentation for argument, if any."
  (getf (pslot (access-result type name) :arg-docs) args-name))

(gen:setf-defun doc-argument-example (type name pslot arg-name example-name)
  "Get/set example. Examples in following form:\
 (list arguments description input)."
  (getf (getf (pslot (access-result type name) :arg-examples) args-name)
	example-name))

(flet ((doc-type-symbol (type name)
	 (case (slot-value (access-result type name) 'type)
	   ((defun defmacro)      'function)
	   ((defvar defparameter) 'variable)
	   (t                     t))))
  (defun doc-string (type name arg-name)
    (case type
      ((defun defmacro defvar defparameter)
       (documentation name (doc-type-symbol type name)))
      (t
       (pslot (access-result type name) :doc-str))))
  (defun (setf doc-string) (to type name arg-name)
    (case type
      ((defun defmacro defvar defparameter)
       (setf (documentation name (doc-type-symbol type name)) to))
      (t
       (setf (pslot (access-result type name) :doc-str) to)))))
  
(defvar *document-links* :all
  "What packages are documented when things link to them.\
 (Defaultly :all, then everything but *document-not-links*.")
(defparameter *document-not-links*
  (mapcar #'find-package
	  '(:common-lisp :sb-impl :sb-kernel :sb-int :sb-loop))
  "If *document-links* is :all, these packages are excluded.")


(defun doc-sym-p (sym)
  "Whether symbol is documented."
  (cond
    ((eql *document-links* :all)
     (not (listfind-package sym *document-not-links*)))
    (t
     (listfind-package sym *document-links*))))

(defvar *document-file* "autodoc.html#"
  "Prepended before the variable names. Defaultly \"autodoc.html#\"\
 Note that the # means that it is for single-file.\
 (Change it if you want multiple files.)")

(defvar *document-arguments* :all)

(defgeneric header (type))

(defmacro header-of (type header) ;TODO number header sizes?
  `(defmethod header ((tp (eql ,type)))
     ,header))
(header-of :object :h3)
(header-of :args :h4)
(header-of :arg  :h4)
(header-of :links :h3)
(header-of :link-package :h4)

(defgeneric autodoc (thing way)
  (:documentation "Automatically document a thing.
If way is a list it is taken to be a sequence of ways attached together.
NOTE TODO i am probably going to remove it"))

;;And the reaction to a list. TODO make more useful reaction to list elts.
(defmethod autodoc (thing (way list))
  (mapcan (lambda (w)
	    (if (listp w)
	      (case (car w)
		(:lml2 (cdr w))
		(t     (error "didn't recognize way ~D" w)))
	      (autodoc thing w)))
	  way))

(defgeneric mention (thing way)
  (:documentation "Mention some object in the auto documentation."))

(defmethod mention ((null null) way)
  )
;;TODO replace string-downcase with case-invert.
;; (So lowercase symbols can be done too.)
(defun mention-a-link (type name)
  (string-downcase (format nil "~D_~D" type name)))
(defun mention-href-link (type name)
  (format nil "~D~D" *document-file* (mention-a-link type name)))

(defun mention-href (type name by)
  `((:a :href ,(mention-href-link  type name))
    ,@by))
(defun mention-a (type name by)
  `((:a :name ,(mention-a-link type name))
    ,(mention-href type name by)))

;;Mentioning code objects.
(defmethod mention ((vec vector) (way (eql :a-lml2))) ;Mention to refer to.
  (let ((type (aref vec 0)) (name (aref vec 1)))
    (mention-a type name 
	       (list(string-downcase (format nil "~D ~D" type name))))))

(defmethod mention ((vec vector) (way (eql :a-lml2*))) ;Mention to refer to.
  (let ((type (aref vec 0)) (name (aref vec 1)))
    (mention-a type name 
	       (list (string-downcase (format nil "~D" name))))))

(defmethod mention ((vec vector) (way (eql :lml2))) ;Mention referring.
  (let ((type (aref vec 0)) (name (aref vec 1)))
    (mention-href type name 
		  (list (string-downcase (format nil "~D" name))))))

(defmethod mention (thing way)
  (with-slots (type name) thing
    (let ((*document-file* (if-use (pslot thing :doc-file)
				   *document-file*)))
      (mention (vector type name) way))))

;;Mention examples. (Might also be gathered with code-examples,
; under the macro that creates it.
(defmethod mention ((ex fun-example) (way symbol))
  (mention (vector '**fun-example (slot-value ex 'name)) way))

;TODO allow user to store locations of previous docs.
(defun mention-thing (type by-symbol &optional (way :lml2))
  (if-let obj (access-result type by-symbol)
    (mention obj way)
    (string-downcase (symbol-name by-symbol))))

(defun mention-fun (symbol &optional (way :lml2))
  "Mention the function/macro attached to symbol.
TODO check for indirections in macros? Hmm, can't automatically note these?"
  (mention-thing '(defun defmacro) symbol way))

(defun mention-var (symbol &optional (way :lml2))
  "Mention the variable attached to symbol."
  (mention-thing '(defvar defparameter defconstant) symbol way))

(defun document-code (code)
  "Documents code, currently consists of just calling format."
  (format nil "~S" code))

(defun document-args (args &key (max-init-len 20))
  "Produces a little text representing the arguments."
  (denest
   (let (mode))
   (collecting ())
   (dolist (a args))
   (typecase a
     (cons ;WARNING backquoting WILL bite you here. WTF Sideeffects!
      (collecting "(")
      (appending
       (case mode
	 ((&key &optional)
	  (let ((res (format nil "~a" (cadr a))))
	    `(,(mention-var (car a) :lml2) " "
	       ,(if (> (length res) max-init-len)
		    "..." (document-code (cadr a))))))
	 (t
	  (document-args a :max-init-len max-init-len))))
      (collecting ") "))
     (symbol
      (collecting
	(case a
	  ((&key &optional &rest &body) ;Just change mode
	   (setf mode a)
	   (string-downcase (symbol-name a)))
	  (t
	   (mention-var a :lml2))) " ")))))

(defmethod autodoc (obj (way (eql :lml2-args)))
  ;;Produces a little text representing the arguments.
  (document-args (slot-value obj 'args)))

(defmethod autodoc (obj (way (eql :lml2-args-bumped)))
  ;;Shifted forward a little.
  `((:p "&nbsp;&nbsp;&nbsp;" ,@(autodoc obj :lml2-args))))

(defun map-arguments (args fun &key mode)
  "Map over the arguments in reading-order."
  (dolist (a args)
    (case a
      ((&key &optional &rest)
       (setf mode a))
      (t
       (case mode
	 ((&key &optional &rest)
	  (funcall fun a mode))
	 (t
	  (if (listp a) (map-arguments a fun) (funcall fun a mode))))))))

;TODO this
(defun document-arg-example (example &key overrule-doc)
  (with-slots (name args body doc) example
    `((:h4 "Example " ,(mention example :a-lml2*)
	    ,@(autodoc example :lml2-args))
       ,@(if-use overrule-doc doc)))) ;Documentation string for example.

(defmethod autodoc ((ex fun-example) (way (eql :lml2-code)))
  (with-slots (args body) example
    (list(document-code `(lambda (,@args) ,@body)))))

(defun document-arg-docs (obj &key with-examples)
  "Document on the argument-special doc-strings."
  (with-slots (args) obj
    (when-let arg-docs (pslot obj :arg-docs)
      `((,(header :args) "Arguments")
	,@(denest ;soo nested :/
	   (collecting ())
	   (map-arguments args
	     (lambda (a mode)
	       (when-let doc (getf arg-docs (if (listp a) (car a) a))
		 (appending
		  `((,(header :arg)
		     ,(case mode
			(&key "Keyword ")
			(&optional "Optional argument ")
			(&rest "Rest argument "))
		      (if (listp a) (car a) a))
		    ,@doc
		    ,@(when (listp a)
		       `((:p "Defaultly: "
			  ,(document-code (cadr a)))))))))))))))

(defmethod autodoc (obj (way (eql :lml2-arg-docs)))
  (document-arg-docs obj))

(defmethod autodoc (obj (way (eql :lml2-arg-docs-examples)))
  (document-arg-docs obj :with-examples t))

(defmethod autodoc (obj (way (eql :lml2-arg-docs)))
  (document-arg-docs obj))

(defun document-links (obj &key (way :lml2))
  "Document useage of function/macros in other functions/macros."
  (denest
   (with-slots (fun-dep) obj
     (setf fun-dep (sort fun-dep #'symbol-package>))) ;Sort by package.
   (collecting (nil own-pkg col-own))
   (collecting (nil rest col-rest))
   ;Need to alter them to only accept the wanted ones, and to mention them.
   (flet ((col-own (collect)
	    (when (and (symbolp collect) (doc-sym-p collect))
	      (col-own (mention-fun collect way))))
	  (col-rest (collect)
	    (col-rest
	     (if (and (symbolp collect) (doc-sym-p collect))
	       (mention-fun collect way)
	       collect)))))
   (let (prev)) ;Store previous.
   (:return ;How to return after.
     (unless (and (null own-pkg) (null rest))
       `((,(header :links) "Used macros and functions")
	 (,@(unless (null own-pkg)
	      `((,(header :link-package) "From package itself")
		,@own-pkg)))
	 ,@(if-use rest "No other packages used."))))
   (dolist (d fun-dep)) ;Iterate.
   (cond
     ((not (doc-sym-p d))
      nil)
     ((same-package obj d) ;TODO what does this do again..
      (when own-pkg (col-own ", "))
      (col-own d))
     ((same-package d prev)
      (col-rest ", ")
      (col-rest d))
     (t
      (setf prev d)
      (col-rest `(,(header :link-package) "From package "
		   ,(format nil "~A" (to-package-name d))))
      (col-rest d)))))

(defmethod autodoc (obj (way (eql :lml2-document-links)))
  (document-links obj :way :lml2))

;Headers that the links end up _to_.
(defmethod autodoc ((tfn track-fun) (way (eql :lml2-link-header)))
  `((:b (:h2 ,(mention tfn :a-lml2)))))

(defmethod autodoc ((tfn track-fun) (way (eql :lml2-link-header*)))
  `((:b (:h2 ,(mention tfn :a-lml2*)))))
;Header that link to there.
(defmethod autodoc ((tfn track-fun) (way (eql :lml2-header)))
  `((,(header :object) ,(mention tfn :lml2))))

;Documentation string.
(defmethod autodoc ((tfn track-fun) (way (eql :doc-str)))
  (with-slots (type name) tfn
    (list
     (case type 
       ((defun defmacro) (documentation name 'function))
       (t                (doc-string type name))))))

;Two suggestions on how to document function-like objects.
(defmethod autodoc ((tfn track-fun) (way (eql :lml2)))
  (autodoc tfn '(:lml2-link-header :lml2-args-bumped
		 :doc-str
		 :lml2-arg-docs
		 :lml2-document-links)))

(defmethod autodoc ((tfn track-fun) (way (eql :lml2-short)))
  (autodoc tfn '(:lml2-header :lml2-args :doc-str)))

(defmethod autodoc  ((tv track-var) (way (eql :lml2)))
  (with-slots (name init) tv
    `((,(header :object) ,(mention tv :a-lml2))
      (:p ,(documentation name 'variable))
      (:p ,(format nil "Initial value ~A" init)))))

(defmethod autodoc ((tv track-var) (way (eql :lml2-short)))
  (autodoc tv :lml2)) ;Same, for now.

(defmethod autodoc ((ht hash-table) way)
  (maphash (lambda (key val)
	     (declare (ignore key))
	     (autodoc val way)) ht))	   

(defmethod autodoc ((all (eql :all)) way)
  (autodoc *scan-result* way))

;;The following is only to mop up various inputs.
;;TODO allow it to set the way?
(defun document-pages (package
		       &key (single-page t)
		       (order '(("Variables" (defvar defparameter))
				("Functions" (defun defmacro)))))
  "Makes pages, contents page, and short versions in lml2."
  (unless (packagep package)
    (setf- find-package package))
  (denest
   (flet ((sort-symname (list)
	    (sort list (lambda (a b)
			 (string< (symbol-name a) (symbol-name b)))))))
   (let ((symbols 
	  (sort-symname (collecting () (do-symbols (sym package)
					 (when (same-package sym package)
					   (collecting sym))))))
	 (external-symbols
	  (sort-symname (collecting () (do-external-symbols (sym package)
					 (collecting sym)))))))
   (collecting ((list) cd col-doc))
   (collecting ((list) cc col-contents))
   (collecting ((list) cs col-short))
   (flet ((documenting (to-be &optional (rel 0))
	    (when single-page
	      (flet ((relative (k)
		       (intern (format nil "H~D" (+ k  rel))
			       (find-package :keyword))))
		(col-doc `(nil (,(relative 2) ,to-be)))
		(col-contents `(,(relative 3) ,to-be))
		(col-short `(nil (,(relative 3) ,to-be))))))
	  (announce (blah)
	    (when single-page
	      (col-doc blah) (mapcar #'col-contents (cdr blah))
	      (col-short blah)))
	  (doc (names sym)
	    (when-let obj (access-result names sym)
	      (col-doc `(,(with-slots (type name) obj
			    (string-downcase(format nil "~D_~D" type name)))
			  ,@(autodoc obj :lml2)))
	      (col-contents `(:p ,(mention obj :lml2)))
	      (col-short `(nil ,@(autodoc obj :lml2-short))))))
     (let ((longer
	    `(nil (:p (:h1 "package " ,(package-name package))
	    (:p ,(documentation package t)) (:hr)))))
       (col-short longer)
       (col-doc longer))
     
     (documenting "External" -1)      
     (dolist (doing order)
       (destructuring-bind (explanation names &optional announce) doing
	 (documenting explanation) ;TODO avoid it if none?
	 (when announce
	   (announce announce))
	 (dolist (sym external-symbols) ;Collect pages.
	   (doc names sym))))
     (documenting "Internal" -1)
     (dolist (doing order)
       (destructuring-bind (explanation names) doing
	 (documenting explanation) ;TODO avoid if none.
	 (dolist (sym symbols) ;Collect pages.
	   (unless (find sym external-symbols)
	     (doc names sym)))))
     (return-from document-pages
       (values (copy-tree cd) (copy-tree cc) (copy-tree cs))))))

(defun document-pages-write
    (package &key single-page single-page-header 
     (order '(("Variables" (defvar defparameter))
	      ("Functions" (defun defmacro))))
     short-version-file; "autodoc-short.html")
     short-version-header (contents-a (list :valign :top)))
  "Writes automatically documented information. "
  (multiple-value-bind (doc contents short)
      (document-pages package
        :single-page (or single-page single-page-header) :order order)
    (mk-website :page-list doc :left contents :left-a contents-a
      :single-page
      (when single-page
	(subseq *document-file* 0 (- (length *document-file*) 1)))
      :single-page-header single-page-header)
 ;TODO the normal version prepended to short. SERIOUS WTF-age.
;I'd say the COLLECTING macro is doing it wrong, but it's output seems
; normal. It seems to be calculated correctly and then be incorrect here
;  anyway.
    (when short-version-file
      (mk-website :page-list short
		  :single-page short-version-file
	:single-page-header short-version-header))))

(defun document (package &key (pre-file "") (directory "") (single-page t)
		 (order '(("Variables" (defvar defparameter))
			  ("Functions and macros" (defun defmacro)))))
  "Document a single package."
  (let ((*default-pathname-defaults*
	 (pathname
	  (format nil "/~{~a/~}~D/"
		  (cdr (pathname-directory *default-pathname-defaults*))
		  directory)))
	(*document-file* 
	 (concatenate 'string pre-file
	    (string-downcase (to-package-name package)) ".html#")))
    (document-pages-write package :single-page single-page :order order)))

(when (find-package :asdf)
  
  (defun document-system
      (sys
       &key (verbose t) version proclamations
       (load-hook
	(lambda (sys)
	  (declare (ignore sys))
	  (values verbose version proclamations)))
       (expression-hook #'scan-expression-hook) (scan-recurse-cnt 0)
       packages ;coerce which packages are documented.
       (single-page t) (pre-file "") (directory "")
       (order '(("Variables" (defvar defparameter))
		("Function" (defun defmacro))))
       (write-recurse-cnt 0)
       (writing-hook
	(lambda (pkg sys)
	  (declare (ignore pkg sys))
	  (values pre-file directory single-page order))))
    (let*((package-by-level
	   (scan-system sys
	      :load-hook load-hook :expression-hook expression-hook
	      :recurse-cnt scan-recurse-cnt)))
     ;Document it.
      (do ((package-by-level (if packages (list packages)
				 package-by-level)
			     (cdr package-by-level))
	   (depth write-recurse-cnt (- depth 1)))
	  ((or (null package-by-level) (< depth 0)) nil)
	(dolist (pkg (car package-by-level))
	  (multiple-value-bind (pre-file directory single-page order)
	      (funcall writing-hook pkg sys)
	    (document pkg :pre-file pre-file :directory directory
	       :single-page single-page :order order))))))
  
  ) ;/when (find-package..

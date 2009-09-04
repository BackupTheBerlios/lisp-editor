
(defpackage :autodoc
  (:use :common-lisp :generic :iterate :lml2 :package-stuff
	:code-scan)
  (:export autodoc arguments-text
	   *package-locations* *pre-file* *directory*
	   *single-file-page* 
	   *single-page-name* *contents-page-name*
	   *doc-creator*
	   
	   *links-inside-file*
	   page-maker-each-own-file page-maker-list
	   page-maker-with-contents-final
	   *links-collect*)
  (:documentation "Makes trackers for code-scan to collect information about\
 code, and provides automatic documentation from this.

Add documentation for your own macros by adding trackers with\
 code-scan:track-input, and adding documentation-creators in\
 *doc-creator* with getf."))

;TODO seperate pages vs bookmarked long pages.

;;Information like available via DOCUMENTATION isn't collected.
;; (As you can get it with that function.)

(in-package :autodoc)

(defvar *package-locations* (make-hash-table)
  "pre-file and directory of other packages.")

(defvar *pre-file* "" "File prepend to put documentation in.")
(defvar *directory* "autodoc/" "Directory to put the documentation in.")

(defvar *document-packages* nil "Packages currently being documented.")

(defvar *single-page-name* "Documentation"
  "Name for page, if it is a single one.")
(defvar *contents-page-name* "Contents"
  "Name for contents page.")

(defvar *links-inside-file* nil
  "Whether links are to within a file.")

(defun documenting-p (name)
  "Whether the function/macro under name is being documented."
  (one-of-packages name *document-packages*))

(defun make-filename (name &optional (directory *directory*)
		      (pre-file *pre-file*))
  "Generates a file name."
  (concatenate 'string
	       directory pre-file
	       (iter (for el in-vector (string-downcase(symbol-name name)))
		     (collect
			 (case el
			   (#\* #\_)
			   (t el))))
	       ".html"))

(defun locate-filename (name)
  "Locates a file name."
  (if-use
   (when (documenting-p name) ;Just one of these.
     (if *links-inside-file*
       (format nil "#~D" (symbol-name name))
       (make-filename name "")))
   (when-let got (gethash (symbol-package name) *package-locations*)
     (destructuring-bind (pre-file directory) got
       (make-filename name directory pre-file)))
   (when (string= (package-name(symbol-package name)) "COMMON-LISP")
      ;TODO hyperspec.
     )))

(defun mention-function (name)
  "Mentions a function, linked if possible."
  (if-let filename (locate-filename name)
    `((:a :href ,filename)
      ,(symbol-name name))
    (symbol-name name)))

(defun full-name-gen (tracker)
  "Generates the full name based on the tracker."
  (with-slots (name type) tracker
    (format nil "~D: ~D" (case type
			      (defun "function") (defmacro "macro"))
	    (string-downcase name))))

;;Using information.
(defun make-header (&key full-name more-meta)
  `(:head
    (:title ,(format nil "~D" full-name))
    ((:meta :http-equiv "Content-Type"
	    :content "text/html; charset=iso-8859-1"))
    ((:meta :name "description"
	    :content ,(format nil "autodoc for ~D" full-name)))
    ,@more-meta))
 
(defun arguments-text (args &key mode)
  "Produces a little text representing the arguments."
  (iter (for a in args)
	(cond ((listp a)
	       (case mode
		 ((&key &optional)
		  (collect (format nil "~S" a)))
		 (t
		  (appending (arguments-text a)))))
	      ((symbolp a)
	       (case a
		 ((&key &optional &rest &body)
		  (setf mode a)))
	       (collect (symbol-name a))))
	(collect " ")))

(defun fun-macro-collect (tracker)
  "Collect used functions/macro of a tracked function for display."
  (with-slots (name) tracker
    (let ((link-list (cdr (assoc name (car *function-links*)))))
    ;Sort the links.
      (setf link-list (sort link-list #'symbol-package>))
    ;Make result.
      (when-let
	  result
	  `(,@(when-let here
		(iter
		  (for link in (iter (for iter on link-list)
				     (when (same-package (car iter) name)
				       (return iter))))
		  (if (same-package link name)
		      (appending `(,(mention-function link) ", "))
		      (finish)))
		`((:h4 "In same package, " ,(to-package-name name))
		  ,@here))
	    ,@(flet
	       ((eat-links (invert &key cur-package want-package)
		 (iter
		   (for link in link-list)
		   (unless (same-package link name)
		     (unless (when cur-package
			       (same-package cur-package link))
		       (setf cur-package link
			     want-package (one-of-packages
					   link *document-packages*))
		       (when invert (setf want-package (not want-package)))
		       (when want-package
			 (collect `(:h4 "In package "
					,(to-package-name link)
					": "))))
		     (when want-package
		       (appending `(,(mention-function link) ", ")))))))
	       `(,@(eat-links nil)
		   ,@(when-let outside (eat-links t)
		       `((:h4 "Outside current documenting-set.")
			 ,@outside)))))
	`(:p
	  (:h3 "Links to other functions/macros")
	  ,@result)))))

(defparameter *links-collect* #'fun-macro-collect
  "Function that collects the links for documentation.")

(flet ((arguments (tracker)
	 `(:p "Arguments: "
	      (:b ,@(arguments-text (slot-value tracker 'args)))))
       (doc-string (tracker)
	 (when-let doc-str (documentation (slot-value tracker 'name) 'function)
	   `((:p ,doc-str))))
       (uses-fun (tracker)
	 (when *links-collect*
	   (funcall *links-collect* tracker)))
       (location (tracker)
	 (with-slots (from-pos) tracker
	   (when (listp from-pos)
	     (case (car from-pos)
	       (:file-line
		`(:p (:h3 "Defined in")
		     ,(format nil "File: ~D, line ~D."
			      (second from-pos) (third from-pos)))))))))
  (defun doc-from-tracker-long (tracker)
    "Produces long documentation about function/macro in tracker."
    `(((:a :name ,(symbol-name (slot-value tracker 'name)))
       (:h2 ,(full-name-gen tracker)))
      ,(arguments tracker) ,@(doc-string tracker)
      ,(uses-fun tracker) ,(location tracker)
      (:hr)))
  (defun doc-from-tracker-short (tracker)
    "Produces short documentation about function/macro in tracker."
    `(:p (:h4 ((:a :href ,(locate-filename (slot-value tracker 'name)))
	       ,(full-name-gen tracker)))
	 ,(arguments tracker) ,(doc-string tracker) (:hr))))

(defvar *doc-creator* nil
  "Documentation makers for things that are not defmacro or defun.")

(defvar *pages-list* nil)

(defun page-maker-list (tracker)
  "Page maker that keeps a list of contents."
  (push tracker *pages-list*))

(defun page-maker-each-own-file (tracker)
  "Simple way of making pages, only uses long version."
  (page-maker-list tracker)
  (eval `(html-file-page (,(make-filename (slot-value tracker 'name)))
	   ,(make-header :full-name (full-name-gen tracker))
	   (:body ,@(doc-from-tracker-long tracker)))))

(defun page-maker-contents-final ()
  "Dedicated contents page."
  (macrolet ((package-aware-iter ((&body package-switch) &body body)
	       "Iterates the list, doing something else each package\
 switch. Saves tonnes of code."
	       (with-gensyms (cur-package)
		 `(let (,cur-package)
		    (iter (for p in *pages-list*)
			  (with-slots (name args) p
			    (unless (when ,cur-package
				      (same-package ,cur-package name))
			      (setf ,cur-package (slot-value p 'name))
			      ,@package-switch)
			    ,@body))))))
    (eval `(html-file-page
	       (,(format nil "~D~D~D"
			 *directory* *pre-file* *contents-page-name*))
	     (:head
	      (:title "Contents page")
	      ((:meta :http-equiv "Content-Type"
		      :content "text/html; charset=iso-8859-1"))
	      ((:meta :name "description"
		      :content "contents page from autodocumentation.")))
	     ,@(package-aware-iter
		((collect `(:h3 "Package " ,(to-package-name name))))
		(collect `(:p ,(mention-function name) " "
			      ,@(arguments-text args))))))))


(defun page-maker-with-contents-final (&key one-page)
  "Page with contents finalizer."
  (setf *pages-list* ;Sort by package and name.
	(sort *pages-list* (lambda (a b)
			     (symbol-package> (slot-value a 'name)
					      (slot-value b 'name)))))
  (macrolet ((package-aware-iter ((&body package-switch) &body body)
	       "Iterates the list, doing something else each package\
 switch. Saves tonnes of code."
	       (with-gensyms (cur-package)
		 `(let (,cur-package)
		    (iter (for p in *pages-list*)
			  (with-slots (name args) p
			    (unless (when ,cur-package
				      (same-package ,cur-package name))
			      (setf ,cur-package (slot-value p 'name))
			      ,@package-switch)
			    ,@body))))))
   ;Make pages with contents on the side. TODO channel more arguments.
    (let ((*links-inside-file* one-page))
      (mk-website:mk-website
       :contents
       (package-aware-iter ;Sidebar-contents.
	((collect `(:h3 "Package " ,(to-package-name name))))
	(collect `(:p ,(mention-function name))))
       :page-list
       (package-aware-iter
	((collect `("" (:h2 "Package " ,(to-package-name name)))))
	(collect`(,(make-filename name) ;List of pages, starting with their
		                        ;filename. (Only used if not one-page.
		   ,(make-header :full-name (full-name-gen p))
		   ,@(mk-website:with-bars
		      nil (doc-from-tracker-long p)))))
       :one-page
       (when one-page
	 (format nil "~D~D~D" *directory* *pre-file* *single-page-name*))
       :one-page-header
       (when one-page
	 `(:head
	   (:title ;TODO probably clumsy format use.
	    ,(format nil "Documentation for ~{~a~^, ~} and ~D."
		     (butlast *document-packages*)
		     (car(last *document-packages*))))
	   ((:meta :http-equiv "Content-Type"
	     :content "text/html; charset=iso-8859-1"))
	   ((:meta :name "description"
	     :content "contents page from autodocumentation.")))))))
  (setf *pages-list* nil))

;;TODO framed pages? They need to be linkable!

(defun autodoc (&key
		(packages *document-packages*)
		(pre-file *pre-file*) (directory *directory*)
		(page-maker #'page-maker-each-own-file)
		(page-maker-final #'page-maker-contents-final))
  "Makes documentation of everything is one of the documentable."
  (let ((*document-packages* packages)
	(*pre-file* pre-file) (*directory* directory))
   ;Checkout all the trackers.
    (maphash (lambda (name tracker)
	       (when (documenting-p name)
		 (funcall page-maker tracker)))
	     *functions*)
    (funcall page-maker-final)
    (do ((c *doc-creator* (cddr c))) ;Other-then defmacro, defun.
	((null c))
      (if-let tracker (gethash (car c) *functions*)
	(funcall (cadr c) tracker)
	(warn "Didn't autodoc with documentation-creator of ~D; tracker did\
 not exist." (car c))))))


;;TODO probably not best way..
(defun track-input-override (name args type track-fun)
  "Manual override to allow you to track without ever code-scanning."
  (setf (gethash name *functions*)
	(make-instance 'function-tracker
		       :name name :type type :args args
		       :track-fun track-fun)))	

;;TODO get useful info.
(track-input-override 'declaim '(&rest decl) 'defmacro
		      (lambda (form)
			(cdr form)))

;Tracker for variables.
(flet ((var-track (from) ;TODO scanner doesn't seem to activate trackers.
	 "Tracker of variables."
	 (list (second form)))
       (add (which fun)
	 (track-input-override
	  which '(var &optional val doc) 'defmacro fun)))
  (add 'defparameter #'var-track)
  (add 'defvar #'var-track))

;And the autodoc for variables.
(flet ((var-doc-creator (tracker)
	 "Creates some documentation with the variables."
	 (let ((full-name "Variables"))
	   (with-slots (name tracked-inputs) tracker
	     (eval
	      `(html-file-page (,(format nil "~D~DVars-of-"
					 *directory* *pre-file*))
		 ,(make-header :full-name full-name)
		 (:h2 ,full-name) ;TODO bookmark (:a ..)
		 (:body
		  ,@(iter
		     (for i in tracked-inputs)
		     (case (car i)
		       (:var
			(collect (:h3 (case name
					(defparameter "Parameter")
					(defvar       "Variable"))
				      (car i)))
			(when-let doc-str (documentation i 'variable)
			  (collect `(:p ,@doc-str)))))))))))))
  (setf (getf *doc-creator* 'defvar) #'var-doc-creator
	(getf *doc-creator* 'defparameter) #'var-doc-creator))

;;Tracker for defclass. (Manually)
(track-input-override
 'defclass '(name direct-superclasses direct-slots &rest options) 'defmacro
 (lambda (form)
   (cdr form)))
;And defstruct.
(track-input-override
 'defstruct '(name-and-options &rest slot-descriptions) 'defmacro
 (lambda (form)
   (cdr form)))

(setf (getf *doc-creator* 'defclass)
      (lambda (tracker)
	)) ;;TODO autodocumentation for classes/structs.
	
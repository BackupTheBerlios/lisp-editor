;;Author: Jasper den Ouden

(cl:in-package :cl)

(defpackage :gil-share
  (:use :gil :common-lisp :generic)
  (:nicknames :gils)
  (:export *stream*
	   *tab-depth* *tab-step* *list-tab-cnt*
	   *cur-char-depth* *line-len* *acceptable-split-ratio*
	   p paragraph
	   point-list alt-point-list
	   dot-list dot-list-style
	   numbered-list *long-number*
	   b i u url-link
	   *link-page-style* link link-pos
	   *cur-page* *pages* page
	   comment
	   wformat call make-gil-definer
	   base-image file-image title file-name
	   mk-split)
  (:documentation "Highly suggested variables, classes and keywords for\
 GIL. Purpose is to standardize these over implementations.
NOTE early stage."))

(in-package :gil-share)

(def-changable-var *tab-depth* :init 0 :doc "Current tab depth.")
(def-changable-var *tab-step* :init 3 :doc "Step in 'spaces' per tab.")

(def-changable-var *line-len* :init 80 :doc "Maximum line length.")
(defvar *cur-char-depth* 0 "Currect character depth.")
(defvar *acceptable-split-ratio* 0.8
  "If limiting line length, the fraction to split that is still\
 acceptable.")

(def-changable-var *list-tab-cnt* :init 1
  :doc "Number of tabs listers make.")

;;Convenience functions.
;;---Lists
(defun p (&rest paragraph)
  "List of paragraphs. (glist with :p)"
  (apply #'glist (cons :p paragraph)))

(defclass paragraph () ;TODO even used?
  ((tabs :initarg :tabs :type fixnum :initform 0
	 :documentation "Number of tabs all over")
   (jump :initarg :jump :type fixnum :initform 0
	 :documentation "Number of spaces at the start."))
  (:documentation "Paragraph possibly all-tabbed, and a tab jump."))

(defun point-list (&rest paragraphs)
  "List of points."
  (apply #'glist (cons :list paragraphs)))
(defun alt-point-list (&rest paragraphs)
  "List of alternative style points."
  (apply #'glist (cons :alt-list paragraphs)))

(defclass dot-list ()
  ((style :initarg :style :reader dot-list-style))
  (:documentation "Dots being represented as some text."))

(def-changable-var *long-number* :init 99
		   :doc "When a number is considered long.")

(defun numbered-list (&rest paragraphs)
  "Numbered list."
  (apply #'glist (cons :numbered-list paragraphs)))

;;---Notes
(defun b (&rest paragraph)
  "Bold text."
  (apply #'note (cons :bold paragraph)))
(defun i (&rest paragraph)
  "Italic text."
  (apply #'note (cons :italic paragraph)))
(defun u (&rest paragraph)
  "Underlined text."
  (apply #'note (cons :underlined paragraph)))

;;---Pages.
(defclass page ()
  ((name :initarg :name :accessor name :initform nil :type symbol)
   (link-ends :initarg :link-ends :initform nil :type list)))

(def-changable-var *pages* :init nil :doc "Current existing pages.")

(defun find-page (page-name)
  "Find a page."
  (declare (type symbol page-name))
  (find-if (lambda (p) (eql (slot-value p 'name) page-name)) *pages*))
(defun get-page (page-name)
  (declare (type (or symbol string) page-name))
  (let ((page-name (if (symbolp page-name) page-name (intern page-name))))
    (if-use (find-page page-name)
	    (car (push (mk page :name page-name) *pages*)))))

(defun find-link (link-name)
  "Finds the page the link belongs to."
  (declare (type symbol link-name))
  (find-if (lambda (p)
	     (find link-name (slot-value p 'link-ends)))
	   *pages*))

(def-changable-var *cur-page* :init nil :doc "Current page.")
(def-changable-var *link-page-style* :init nil :doc "Way the link is\
 followed with regard to page, may try to open new tab or replace old,
 etcetera..")

;;---Links as action.
(defclass link ()
  ((page :initarg :page :accessor page :type (or null page))
   (name :initarg :name :accessor name :type symbol))
  (:documentation "Link to a position."))

(defun intern* (x)
  (if (stringp x) (intern x) x))

(defun link (object link-name &optional page)
  "Adds action to link to an object."
  (let*((link-name (intern* link-name))
	(page (typecase page
		(null   (find-link link-name))
		(string (get-page page))
		(symbol (get-page page))
		(t      page))))
    (when page
      (pushnew link-name (slot-value page 'link-ends)))
    (action (mk link :page page :name link-name) object)))

(defclass url-link ()
  ((name :initform "" :initarg :name :type string :reader name)))

(defun url-link (name object)
  "Link to the outside with url. Avoid linking inside via url!"
  (action (mk url-link :name name) object))

;;And link-positions; annotations that links can go there by some name.
(defun link-pos (object link-name)
  "Adds a notation that links can go here."
  (let ((link-name (intern* link-name)))
    (apply #'note (list (mk link :name link-name)
			object))))

;;Some declaims.
(declaim (inline p b i u link link-pos))

;;Function to help implement.
(defmacro wformat (str &rest args)
  "Format writer."
  `(progn
     (when (= *cur-char-depth* 0)
       (dotimes (k *tab-depth*)
	 (write-char #\Space)))
     (format *standard-output* ,str ,@args)))

(defun call (obj)
  (if (functionp obj)
    (funcall obj) (wformat obj)))

(defmacro make-gil-definer
    (lang-name mac-name lambda-mac
     &key (has-docstr '(and (cdr body) (stringp(car body))))
          (strip-body `(if ,has-docstr (cdr body) body)))
  "Makes a def-gil-method for a language."
  `(progn
     (defmacro ,mac-name (name sep-type (&rest args) &body body)
  "Defines a method with the language argument filled for you. Meant for use
 internal to gil-html only."
  (with-gensyms (sep)
    `(defmethod ,name
	 ((lang (eql ,,lang-name))
	  ,(cond
	    ((or (numberp sep-type) (keywordp sep-type))
	     `(,sep (eql ,sep-type)))
	    ((and sep-type (symbolp sep-type))
	     sep-type)
	    ((listp sep-type)
	     sep-type))
	  ,@args)
       ,@,strip-body)))
     (defmacro ,lambda-mac (name sep-type (&rest args) &body body)
       `(,',mac-name ,name ,sep-type (,@args) ,@(when ,has-docstr (car body))
	  (lambda () ,@,strip-body)))))

;;Images.

(defclass base-image ()
  ((title :initarg :title :initform "" :accessor image-title))
  (:documentation "Base class for images."))

(defclass file-image (base-image)
  ((file-name :initarg :filename :accessor file-name))
  (:documentation "Images named by file."))

(defun comment (&rest code)
  (apply #'note (cons :comment code)))

;;Separation in parts.

(defclass split ()
  ((spacing :initarg :spacing :initform nil :type list)
   (way :initarg :way
	:initform :relative :type (or (eql :relative)
				      (eql :absolute)))
   (dir :initarg :dir :initform :v :type (or (eql :v) (eql :h)))))

(defun mk-split (splits &key (dir :v) (way :relative))
  "Makes a split screen for input into glist."
  (make-instance 'split :spacing splits :dir dir :way way))

(defmethod i-glist ((lang (eql :check)) (split split) (frames list))
  (with-slots (spacing way) split
    (unless (= (length frames) (+ (length spacing)
				  (if (eql gils::way :absolute) 1 0)))
      (error "There must be a start of each frame~s"
	     (if (eql gils::way :absolute)
	       " and one for the ending when absolute." ".")))))

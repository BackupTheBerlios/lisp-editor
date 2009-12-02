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
	   b i u link link-name link-page
	   *link-page-style* link link-pos
	   *cur-page* *pages*
	   wformat call make-gil-definer
	   base-image file-image title file-name)
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

(defun r-glist (sep paragraph)
  (apply #'glist (cons sep paragraph)))

;;Convenience functions.
;;---Lists
(defun p (&rest paragraph)
  "List of paragraphs. (glist with :p)"
  (r-glist :p paragraph))

(defclass paragraph ()
  ((tabs :initarg :tabs :type fixnum :initform 0
	 :documentation "Number of tabs all over")
   (jump :initarg :jump :type fixnum :initform 0
	 :documentation "Number of spaces at the start."))
  (:documentation "Paragraph possibly all-tabbed, and a tab jump."))

(defun point-list (&rest paragraphs)
  "List of points."
  (r-glist :list paragraphs))
(defun alt-point-list (&rest paragraphs)
  "List of alternative style points."
  (r-glist :alt-list paragraphs))

(defclass dot-list ()
  ((style :initarg :style :reader dot-list-style))
  (:documentation "Dots being represented as some text."))

(def-changable-var *long-number* :init 99
		   :doc "When a number is considered long.")

(defun numbered-list (&rest paragraphs)
  "Numbered list."
  (r-glist :numbered-list paragraphs))

;;---Notes
(defun b (&rest paragraph)
  "Bold text."
  (i-note *lang* :bold (r-glist :series paragraph)))

(defun i (&rest paragraph)
  "Italic text."
  (i-note *lang* :italic (r-glist :series paragraph)))
(defun u (&rest paragraph)
  "Underlined text."
  (i-note *lang* :underlined (r-glist :series paragraph)))

(defclass link ()
  ((page :initarg :page :accessor link-page :type symbol)
   (name :initarg :name :accessor link-name :type symbol))
  (:documentation "Link to a position."))

;;---Links as action.
(def-changable-var *pages* :init nil :doc "Current existing pages.")
(def-changable-var *cur-page* :init nil :doc "Current page.")
(def-changable-var *link-page-style* :init nil :doc "Way the link is\
 followed with regard to page, may try to open new tab or replace old,
 etcetera..")

(defun link (object link-name &optional (page-name *cur-page*))
  "Adds action to link to an object."
  (pushnew page-name *pages*)
  (action (mk link :page page-name :name link-name) object))

;;And link-positions; annotations that links can go there by some name.
(defun link-pos (object link-name)
  "Adds a notation that links can go here."
  (note (mk link :page *cur-page* :name link-name) object))

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
    `(defmethod ,name ((lang (eql ,,lang-name))
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

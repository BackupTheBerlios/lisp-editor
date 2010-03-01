;;
;;  Copyright (C) 2010-03-01 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

;;Note messy file.

(cl:in-package :cl)

(defpackage :gil-share
  (:use :gil :common-lisp :generic)
  (:nicknames :gils)
  (:export *indent-depth* *indent-step* *list-tab-cnt*
	   *cur-char-depth* *line-len* *acceptable-split-ratio*
	   *long-number*
	   *handle-page*
	   p point-list alt-point-list numbered-list
	   dot-list
	   
	   b i u comment p-code code
	   url-link link-pos
	   header section *section-level-modifier*
	   *link-page-style* link follow-link
	   *cur-page* 
	   
	   timestamp
	   
	   newline hr
	   
	   wformat
	   base-image file-image
	   mk-split
	   table table-el)
  (:documentation "Highly suggested variables, classes and keywords for\
 GIL. Purpose is to standardize these over implementations.
NOTE early stage.
TODO improve the messyness of the file, split out some stuff."))

(in-package :gil-share)

(defun intern* (x &optional (pkg (find-package :gil-share)))
  (if (stringp x) (intern x pkg) x))

(defvar *indent-depth* 0
  "Current tab depth.")
(defvar *indent-step* 3
  "Step in 'spaces' for indentation.")
(defvar *line-len* 80
  "Maximum line length.")
(defvar *cur-char-depth* 0
  "Current character depth.")
(defvar *acceptable-line-split-ratio* 0.8
  "If limiting line length, the fraction to split that is still\
 acceptable. TODO more apt name")

(defvar *attempt-readable* t
  "Whether the output/implementation should try produce human-readable\
 results.")
(defvar *attempt-shorten* t
  "Whether to try make it as short as possible.")

(defvar *list-tab-cnt* 1 "Number of tabs listers make.")

(defvar *page-path* (make-hash-table)
  "Hash table with page names that want an alternate path.\
 (only do it for a reason.)")
(defvar *handle-page* #'identity
  "Things that have to be done around a page.")

(defvar *cur-page* ""
  "Current page.")
(defvar *link-page-style* nil "Way the link is\
 followed with regard to page, may try to open new tab or replace old,
 etcetera..")

(defvar *timestamp* (get-universal-time))
(defun timestamp ()
  (multiple-value-bind
	(second minute hour date month year day daylight-p zone)
      (decode-universal-time *timestamp*)
    (declare (ignore date daylight-p zone))
    (format nil "~D:~D:~D ~D-~D-~D" 
	    hour minute second day month year)))

;;Convenience functions.
(defmacro def-glist-caller (name (&rest args) &body body)
  "Defines a function that calls i-glist with structure depending on args."
  (with-gensyms (objects)
    `(defun ,name (,@args &rest ,objects)
       ,@(butlast body)
       (glist-list ,(car(last body)) ,objects))))

;;---Lists
(def-glist-caller p () "List of paragraphs. (glist with :p)" :p)
(def-glist-caller point-list () "List of points." :list)
(def-glist-caller alt-point-list () 
  "List of alternative style points." :alt-list)

(defclass dot-list ()
  ((style :initarg :style))
  (:documentation "Dots being represented as some text."))

(defvar *long-number* 99 "When a number is considered long.")

(def-glist-caller numbered-list () "Numbered list." :numbered-list)

(defun newline ()
  (glist :newline))
(defun hr ()
  (glist :horizontal-ruler))

;;---Notes
(def-glist-caller b () "Bold text." :bold)
(def-glist-caller i () "Italic text." :italic)
(def-glist-caller u () "Underlined text." :underlined)

(def-glist-caller comment () "Comment, hidden." :comment)

(defmethod i-glist (lang (way (eql :comment)) (list list))
  (declare (ignore lang way list))
  (lambda ()))

(def-glist-caller code () "Note that it is code." :code)
(def-glist-caller p-code () "Note that it is code in paragraph." :p-code)

;;---Sections and headers

(defvar *section-level-modifier* 0)

(defclass header ()
  ((level :initarg :level :initform 0 :type fixnum)))

(defun header (level &rest objects)
  "A title of a paragraph/other."
  (glist-list (mk header :level level) objects))

(defclass section (header)
  ((name :initarg :name)
   (title :initarg :title)))

(defmethod i-glist (lang (section section) (objects list))
  (with-slots (level name title) section
    (i-glist *lang* :series
      (cons (i-glist *lang* (mk header :level level) title) objects))))

(defun section (level name title &rest objects)
  "Makes a section. If title NULL, will use name, if name NULL, it is just\
 a header."
  (if name
    (glist-list
     (mk section :level level :name name :title (if-use title name))
     objects)
    (glist-list :series (cons (header level title) objects))))

;;Some page-stuff
(defvar *section-page-level* 1)

(defclass link ()
  ((name :initarg :name :accessor name :type symbol))
  (:documentation "Noting a position."))

(defclass follow-link (link) () (:documentation "Go to noted position."))

(defun link (link-name &rest objects)
  "Follow-link."
  (glist-list (mk follow-link :name (intern* link-name))
	      (if-use objects (list (format nil "~a" link-name)))))

;;And link-positions; annotations that links can go there by some name.
(defun link-pos (link-name &rest objects)
  "Adds a notation that links can go here."
  (glist-list (mk link :name (intern* link-name (find-package :gils)))
	      (if-use objects
		      (list (format nil "~a" link-name)))))

(defclass url-link ()
  ((name :initform "" :initarg :name :type string :reader name))
  (:documentation "Link to url."))

(defun url-link (name &rest objects)
  "Link to the outside with url. Avoid linking inside via url!"
  (glist-list (mk url-link :name name)
    (if-use objects
	    (list (format nil "~a" name)))))

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

;;Images.

(defclass base-image ()
  ((title :initarg :title :initform "" :accessor image-title))
  (:documentation "Base class for images."))

(defclass file-image (base-image)
  ((file-name :initarg :filename :accessor file-name))
  (:documentation "Images named by file."))

;;Separation in parts.

(defclass split ()
  ((spacing :initarg :spacing :initform nil :type list)
   (way :initarg :way
	:initform :relative :type (or (eql :relative)
				      (eql :absolute)))
   (dir :initarg :dir :initform :v :type (or (eql :v) (eql :h))))
  (:documentation "Splits the whole thing into separate parts.\
 Not to be used for tables.
TODO do tables, think about whether split makes sense as is.."))

(defun mk-split (splits &key (dir :v) (way :relative))
  "Makes a split screen for input into glist."
  (make-instance 'split :spacing splits :dir dir :way way))

(defmethod i-glist ((lang (eql :check)) (split split) (frames list))
  (with-slots (spacing way) split
    (assert (= (length frames) (+ (length spacing)
				  (if (eql gils::way :absolute) 1 0)))
	    nil "There must be a start of each frame~s"
	    (if (eql gils::way :absolute)
		" and one for the ending when absolute." "."))))

(defclass table ()
  ((cellpadding :initarg :cellpadding :initform nil)
   (cellspacing :initarg :cellspacing :initform nil)
   (border :initarg :border :initform nil)
   (frame  :initarg :frame  :initform nil)
   (rules  :initarg :rules  :initform nil)
   (width  :initarg :width  :initform nil)))

(defvar *in-table* nil)

(defclass table-el ()
  ((x-size :initarg :x-size :initform nil)
   (y-size :initarg :y-size :initform nil)
   (x-align :initarg :x-align :initform nil)
   (y-align :initarg :y-align :initform nil)
   (x-span :initarg :x-span :initform nil)
   (y-span :initarg :y-span :initform nil)

   (contents :initarg :contents :initform nil))
  (:documentation "Table element. *only* in tables!"))

(defun table (&rest elements)
  "Makes a table, table-elements allowed inside."
  (glist-list (mk table) elements))

(defun table-el (properties &rest contents)
  "Element for tables"
  (destructuring-bind (&key width height align valign colspan rowspan
		       (x-size width) (y-size height)
		       (x-align align) (y-align valign) 
		       (x-span colspan) (y-span rowspan))
      properties
    (make-instance 'table-el
      :x-size x-size :y-size y-size
      :x-align x-align :y-align y-align
      :x-span x-span :y-span y-span :contents contents)))
    
(defmethod i-glist (lang (table-el table-el) things)
  (declare (ignore lang things))
  (error "Table elements are to be caught with CALL."))

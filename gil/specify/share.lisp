;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

;;Note messy file.

(cl:in-package :cl-user)

(defpackage :gil-share
  (:use :gil :gil-vars :common-lisp :generic)
  (:nicknames :gils)
  (:export about-file

           p series

	   b i u p-code code quotation
	   notable note comment
	   
	   url-link link-pos
	   
	   lister point-list alt-point-list numbered-list
	   
	   header section
	   link follow-link
	   
	   base-image file-image
	   table table-el col-table)
  (:documentation "Various specifying objects and directly attached\
 functions to help use them."))

(in-package :gil-share)

(defmacro about-file (&rest rest)
  "Should be at start of file, so data about file can be in it."
  (declare (ignore rest)))

;;Convenience functions.
(defmacro def-glist-caller (name (&rest args) &body body)
  "Defines a function that calls i-glist with structure depending only\
 on arguments."
  (with-gensyms (objects)
    `(defun ,name (,@args &rest ,objects)
       ,@(butlast body)
       (glist-list ,(car(last body)) ,objects))))

;;---Lists
(def-glist-caller p () "List of paragraphs." :p)
(def-glist-caller series () "Just chains the objects." :series)
(def-glist-caller point-list () "List of points." :list)
(def-glist-caller alt-point-list () 
  "List of alternative style points." :alt-list)
(def-glist-caller description-list ()
  "List of described stuff" :descriptions)

(defmacro def-glist-ignore (way)
  "Default behavior to ignore something, but not the objects."
  `(defmethod i-glist (lang (way ,way) (objects list))
     (declare (ignore lang way))
     (call-list objects)))

(defclass lister ()
  ((style :initarg :style))
  (:documentation "Dots being represented as some text."))

(def-glist-caller numbered-list () "Numbered list." :numbered-list)

(def-glist-caller b () "Bold text." :bold)
(def-glist-caller i () "Italic text." :italic)
(def-glist-caller u () "Underlined text." :underlined)

(defvar *make-notable-pos* nil
  "Whether to make things noted notable positions to link to.
TODO implement.")

(def-glist-caller notable ()
  "Makes something notable.(Might turn up in index.)"
  :notable)
(def-glist-ignore (eql :notable))

(def-glist-caller note () "Makes something a note, as in something between\
 parenthesis, _not_ as in notable!" :note)
(defmethod i-glist (lang (note (eql :note)) objects)
  (declare (ignore lang))
  (call "(") (call-list objects) (call ")"))

(def-glist-caller comment () "Comment, hidden." :comment)

(defmethod i-glist (lang (way (eql :comment)) (list list))
  (declare (ignore lang way list))
  ) ;This also ignores the objects.

(def-glist-caller code () "Note that it is code." :code)
(def-glist-caller quotation () "Quotation" :quotation)
(def-glist-caller p-code () "Note that it is code in paragraph." :p-code)

;;---Sections and headers

(defclass header ()
  ((level :initarg :level :initform 0 :type fixnum))
  (:documentation
   "Title of something, but without any sort other effect."))

(defun header (level &rest objects)
  "A title of a paragraph/other."
  (glist-list (mk header :level level) objects))

(defclass section (header)
  ((name :initarg :name)
   (title :initarg :title))
  (:documentation
   "Sections are titled parts, that might be paginated separately based on\
 their level."))

(defmethod i-glist (lang (section section) (objects list))
  (with-slots (level name title) section
    (i-glist *lang* :series
      (cons (header level title) objects))))

(defun section (level name title &rest objects)
  "Makes a section. If title NULL, will use name, if name NULL, it is just\
 a header."
  (if name
    (glist-list
     (mk section :level level :name name :title (if-use title name))
     objects)
    (glist-list :series (cons (header level title) objects))))

;;Some page-stuff

(defclass link ()
  ((name :initarg :name :accessor name :type symbol))
  (:documentation "Denote this position."))

(def-glist-ignore link)

(defclass follow-link (link) () (:documentation "Go to noted position."))

(defun link (link-name &rest objects)
  "Follow-link."
  (glist-list (mk follow-link :name (intern* link-name :gil-share))
	      (if-use objects (list (format nil "~a" link-name)))))

;;And link-positions; annotations that links can go there by some name.
(defun link-pos (link-name &rest objects)
  "Adds a notation that links can go here."
  (glist-list (mk link :name (intern* link-name :gils-share))
	      (if-use objects
		      (list (format nil "~a" link-name)))))

(defclass url-link ()
  ((name :initform "" :initarg :name :type string :reader name))
  (:documentation "Link to (presumably outside) url."))

(def-glist-ignore url-link)

(defun url-link (name &rest objects)
  "Link to the outside with url. Avoid linking inside via url!"
  (glist-list (mk url-link :name name)
    (if-use objects
	    (list (format nil "~a" name)))))

;;Some declaims.
(declaim (inline p b i u link link-pos))

;;Images.

(defclass base-image ()
  ((title :initarg :title :initform "" :accessor image-title))
  (:documentation "Base class for images."))

(defclass file-image (base-image)
  ((file-name :initarg :filename :accessor file-name))
  (:documentation "Images named by file."))

;;Separation in parts.

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

(defclass col-table (table)
  ((cols :initform :cols :type list))
  (:documentation "Column table, things about the elements is specified\
 per-column.(Although table-el can still override."O))

(defun col-table (cols &rest list)
  "Column-table, elements specified per-colomn, table-el can still\
 override. Cols is keyword arguments for making an table-el"
  (glist (mk col-table :cols cols)	 
	 list))

;Default behavior of col-table via regular table.
(defmethod i-glist (lang (table col-table) (list list))
  (declare (ignore lang))
  (let ((cols (slot-value table 'cols)))
    (call (glist-list (change-class table 'table)
	    (mapcar
	     (lambda (el col)
	       (mapcar (lambda (s-el)
			 (typecase s-el
			   (table-el ;Overriding
			    s-el)
			   (t ;Make one based on column.
			    (glist
			     (apply #'make-instance `(table-el ,@col))
			     s-el))))
		       el))
	     list cols)))))

;;Calls not accepted.
(defmethod i-call (lang (sym (eql :tableofcontents)))
  (declare (ignore lang))
  (error "Object :tableofcontents does not exist, you looking for\
 :table-of-contents ?(coming from latex?)"))

;;Need it read with *lang* :info and result in *contents* !
;(defmethod i-call (lang (sym (eql :table-of-contents)))
;  (declare (ignore lang))
;  (call(gil-contents:use-contents gil-info::*contents*)))

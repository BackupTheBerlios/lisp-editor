;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage gil-html
  (:use :common-lisp :generic :denest 
	:gil-output-util
	:gil :gil-share :gil-style :gil-vars)
  (:documentation "Gil->html, not that the files linked internally are all 
tracked by gil-info.")
  (:export cur-link-url))

(in-package :gil-html)

(def-glist (way t) objects
  (glist-list way (mapcar #'call objects)))

(defun may-indent ()
  (when gils::*attempt-readable* (indent)))

;;Some utility

(defparameter *default-style-file* "default.css")

(defvar *refer-style* nil)
(defvar *inline-style* nil)

(defun style (&optional (file *default-style-file*))
  (wformat "<link rel=\"stylesheet\" type=\"text/css\" href=\"~a\" />~%"
	   file))

(defmacro surround (with &body body)
  `(surround-fn ,with (lambda () ,@body)))

;NOTE: nothing really has accepts-style true; i don't trust it, why does
;      class not work on ul, and style does, etcetera.. ><
(defun surround-fn (with &optional fill)
  "Puts xml thingies around, if core also adds style."
  (let ((style-str (concatenate 'string
		     (when *refer-style*
		       (format nil " class=\"~a\"" *refer-style*))
		     (when *inline-style*
		       (format nil " style=\"~a\"" *inline-style*))))
	(*refer-style*  nil) ;Turn them off.
	(*inline-style* nil))
    (cond
;      ((and (not accepts-style) (> (length style-str) 0))
;       (surround (format nil "span ~a" style-str)
;	 (surround-fn with fill)))
      (gils::*attempt-readable*
       (indent)
       (wformat (if fill "<~a~a>~%" "<~a~a \>~%") with style-str)
       (when fill
	 (let ((*indent-depth* (+ *indent-depth* 1)))
	   (funcall fill))
	 (write-char #\Newline) (indent)
	 (wformat "</~a>~%" (subseq with 0 (position #\Space with)))))
      (t
       (wformat (if fill "<~a~a>" "<~a~a \>") with style-str)
       (when fill
	 (funcall fill)
	 (wformat "</~a>" (subseq with 0 (position #\Space with))))))))

(defmacro def-surrounding-glist (way with &optional accepts-style)
  (with-gensyms (objects)
    `(def-glist ,way ,objects
       (surround ,with (call-list ,objects)))))

(def-call (fun function) (may-indent) (funcall fun))
(def-call (null null) (declare (ignore null)))

(def-call (str string)
  (may-indent)
  (let ((string (if (and gils::*attempt-shorten*
		         (not gils::*attempt-readable*))
		  (remove-gratuous-whitespace str)
		  str)))
    (if (or *refer-style* *inline-style*)
      (surround "span" (write-string string))
      (write-string string))))

(def-call (num number) (may-indent) (call (format nil "~a" num)))
(def-call anything (error "Not recognized: ~s" anything))

(def-call (symbol symbol)
  (case symbol
    ((:newline :nbnl :non-breaking-newline)
     (wformat "<br\>"))
    ((:space :nbsp :non-breaking-space)
     (wformat "&nbsp;"))
    ((:hr :horizontal-ruler)
     (surround-fn "hr")) ;HAS to use surround in order to pick up styles!
    (t
     (if (keywordp symbol) ;Assume it is an html entity.
       (call (format nil "&~a;" (string-downcase symbol)))
       (error "Symbol not recognized. ~s" symbol)))))

;;Styles.

(def-glist :style-list list ;TODO eh need to supersede the first time :/
  "Style lists go straight to the .css file."
  (with-open-file (css *default-style-file* :direction :output
		   :if-exists :append :if-does-not-exist :create)
    (mapcar (lambda (el) (write-line el css)) list)))

(def-glist (style refer-style) objects
  (let ((*refer-style* (slot-value style 'refer)))
    (if (null (cdr objects))
      (call (car objects)) ;One object; imbed style.
      (surround "span" (call-list objects)))))

(def-glist (style inline-style) objects
  (let ((*inline-style* (slot-value style 'style)))
    (if (null (cdr objects))
      (call (car objects))
      (surround "span" (call-list objects)))))

;;List-like.

(def-surrounding-glist :p "p")

;TODO optionally make link-posses to indexed stuff.
;(defvar *notable-name* "_note~a")
;(defvar *notable-index* 0)

;TODO make exact location links to these togleable
(def-glist (notable (eql :notable)) list 
  (call-list list))

(def-glist :comment list)
(def-glist :note objects
  (wformat "(") (call-list objects) (wformat ")"))

(def-glist :series list
  (call-list list))
(def-glist :header list
  (call-list list))

(def-glist (style symbol) list
  (unless (null list)
    (call(glist-list (mk lister :style style) list))))

;;Lister here recognizes for styles (corresponding CSS.):
;;  none, circle, disc, square
;; Numberings:
;;  armenian, decimal, decimal-leading-zero, georgian, lower-alpha, 
;;  lower-greek, lower-latin, lower-roman, upper-alpha, 
;;  upper-latin, upper-roman

(def-glist (sep lister) list
  (unless (null list)
    (let ((style (slot-value sep 'gils::style)))
      (surround (format nil (case style
			      ((:numbered-list
				:armenian :decimal :decimal-leading-zero 
				:georgian :lower-alpha :lower-greek
				:lower-latin :lower-roman :upper-alpha
				:upper-latin :upper-roman)
			       "ol style=\"~D\"")
			      (t "ul style=\"~D\""))
			(if (symbolp style)
			  (symbol-name
			   (case style
			     (:numbered-list  :decimal)
			     (:list           :disc)
			     (:alt-list       :square)
			     (t style))) "DISC"))
	 (dolist (el list)
	   (surround "li" (call el)))))))

(def-glist :descriptions list ;List of descriptions.
  (call (glist-list (mk table)
	  (mapcar (lambda (el)
		    (b(car el)) (glist-list :series (cdr el)))
		  list))))

;;Links as actions and notes.

(defun sanitized-link (link &rest args)
  "TODO not very sturdy.."
  (substitute #\_ #\* (apply #'format `(nil ,link ,@args))))

(defun cur-link-url (name &key (page (gil-info::get-link-page name)))
  (typecase page
    (gil-info::link-entry ;TODO recognize current page.
     (let ((page (slot-value page 'gil-info::page)))
       (cond ;;NOTE _nasty_ if bugged!!!
	 ((string= *cur-page* page)
	  (sanitized-link "#~a" name))
	 (t;(string= *cur-page* "")
	  (sanitized-link "~a.html#~a" page name)))))
    (gil-info::url-entry
     (slot-value page 'gil-info::url))
    (null
     (warn "Couldn't get page of ~s" (gil-info::link-to-keyword name))
     (sanitized-link "~a.html" name))
    (t
     (error "~a" page))))

(def-glist (link follow-link) objects
  (if (gils::name link)
    (surround (format nil "a href=\"~a\"" (cur-link-url (gils::name link)))
      (call-list objects))
    (call-list objects)))

(def-glist (link link) objects
  (if (gils::name link) 
    (surround (sanitized-link "a name=\"~a\"" (gils::name link))
      (call-list objects))
    (call-list objects)))

(def-surrounding-glist (url url-link)
    (format nil "a href=\"~a\"" (gils::name url)))

;;Basic modifiers.

(def-surrounding-glist :bold "b")
(def-surrounding-glist :italic "i")
(def-surrounding-glist :underlined "u")
(def-surrounding-glist :code "code")
;Oh, yeah, html didnt preserve it here.. Kindah silly.
(def-glist :p-code objects
  (surround "code" ;Don't steal the whitespace.
    (surround "pre"
      (let ((gils::*attempt-shorten* nil)) (call-list objects)))))

;;Headers & Sections.

(def-glist (header header) objects
  (surround (format nil "h~D" (clamp (slot-value header 'gils::level) 1 6))
    (call-list objects)))

(def-glist (section section) objects
  (denest
   (with-slots (gils::level gils::name gils::title) section)
   (let ((*cur-pos* gils::name)))
   (flet ((result ()
	    (glist-list :series
	      (cons (link-pos gils::name
		      (header gils::level gils::title))
		    objects)))))
   (cond ;Note: link wraps round whole thing.
     ((> gils::level gils::*section-page-level*)
      (call (result)))
     (t
      (assert gils::name nil ;TODO get around it?
	      "To paginate a section, it _must_ have a name.")
      (let*((*cur-page*; (gils::intern* gils::name))
	     (if-let path (gethash gils::name gils::*page-path*)
	       (format nil "~a~a" path gils::name) ;Path specified.
	       gils::name))
	    (*default-pathname-defaults*
	     (if-use (gethash gils::name gils::*page-path*)
		     *default-pathname-defaults*)))
	(with-open-file
	    (*standard-output* (sanitized-link "~a.html" gils::name)
		:direction :output
		:if-exists :supersede :if-does-not-exist :create)
	  (surround "head" ;TODO better header making.
	    (surround "title" (call gils::title))
	    (style))
	  (let ((*indent-depth* 0))
	    (call (funcall *handle-page* (result))))))
      ;Return a notice of a linked section.
      (call
       (p(link gils::name
	   (format nil "-> section ~D has own page" gils::name))))))))

;;Images. 
(def-glist (image base-image) objects
  (declare (ignore objects))
  (warn "Html output(at least this version) doesn't know what to do with\
 image type ~D." (type-of image)))

(def-glist (image file-image) objects
  (declare (ignore objects)) ;TODO make title, include more html features.
  (wformat "<img src=\"~D\" \>" (slot-value image 'gils::file-name)))

;;'Windows'.

(defun list-format (str &rest args)
  (list (apply #'format `(,nil ,str ,@args))))

(defmacro tagdata 
    (value &optional (data-name (string-downcase (symbol-name value))))
  `(when ,value
     (list-format "~a=~a" ,data-name ,value)))

;;Tables.

(def-call (table-el table-el)
  (assert gils::*in-table* nil "Table elements _must_ be in a table!")
  (with-slots (gils::x-size gils::y-size
	       gils::x-align gils::y-align gils::x-span gils::y-span
	       gils::contents) 
      table-el
    (let ((gils::*in-table* nil))
      (surround
       (format nil "td~{ ~a~}"
	       (append
		(tagdata gils::x-size "width")
		(tagdata gils::y-size "height")
		(when-let align
		    (case gils::x-align
		      ((:low :left) "left")
		      ((:high :right) "right")
		      ((:center) "center")
		      ((:justify) "center"))
		  (list-format "align=\"~a\"" align))
		(when-let valign
		    (case gils::y-align
		      ((:low :bottom) "bottom")
		      ((:high :top) "top")
		      ((:center :middle) "middle")
		      ((:baseline) "baseline"))
		  (list-format "valign=\"~a\"" valign))
		(tagdata gils::x-span "colspan")
		(tagdata gils::y-span "rowspan")))
       (call-list gils::contents)))))

(def-glist (table table) elements
  (with-slots (gils::border gils::frame gils::rules gils::width
	       gils::cellpadding gils::cellspacing) table
    (let ((gils::*in-table* t))
      (flet ((handle-el (el)
	       (cond
		 ((listp el)
		  (error "Tables are only 2 dimensional.\
 (dont nest lists)"))
		 ((or (stringp el) (numberp el) (functionp el))
		  (surround "td" (call el)))
		 (t ;If type table-el, call above will catch it.
		  (call el)))))
	(surround (format nil "table~{ ~a~}"
		    (append
		     (tagdata gils::cellpadding)
		     (tagdata gils::cellspacing)
		     (tagdata gils::border) (tagdata gils::frame)
		     (tagdata gils::rules)  (tagdata gils::width)))
          (dolist (el elements)
	    (surround "tr"
	      (cond ((listp el) (mapcar #'handle-el el))
		    (t          (handle-el el))))))))))

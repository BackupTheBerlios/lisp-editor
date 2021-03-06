;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage gil-html
  (:use :common-lisp :alexandria :generic :denest :path-stuff
	:gil-output-util
	:gil :gil-share :gil-style :gil-vars :gil-comms)
  (:documentation "Gil->html, not that the files linked internally are all 
tracked by gil-info.")
  (:export link-url))

(in-package :gil-html)

(gil::basic-lang :html)

(defun may-indent ()
  (when *attempt-readable* (indent)))

;;Some utility

(defvar *default-style-file* "default.css")
(defvar *style-went-to* nil)

(defvar *refer-style* nil)
(defvar *inline-style* nil)

(defun style (&optional (file *default-style-file*))
  (wformat "<link rel=\"stylesheet\" type=\"text/css\" href=\"~a\" />~%"
	   (if *style-went-to*
	     (from-path-root (format nil "~a~a" *style-went-to* file)
			     *following-directory*)
	     file)))

(from-path-root "website/meh" "autodoc/")

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
      (*attempt-readable*
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

;(def-call (fun function) (may-indent) (funcall fun))
(def-call (null null) (declare (ignore null)))

(def-call (str string)
  (may-indent)
  (let ((string (if (and *attempt-shorten*
		         (not *attempt-readable*))
		  (remove-gratuous-whitespace str)
		  str)))
    (if (or *refer-style* *inline-style*)
      (surround "span" (write-string string))
      (write-string string))))

(def-call (num number) (may-indent) (call (format nil "~a" num)))

(def-call (null null))

(def-call (symbol symbol)
  (case symbol
    ((:newline :nbnl :non-breaking-newline)
     (wformat "<br>"))
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
  (setq *style-went-to* *following-directory*)
  (with-open-file
      (css (concatenate 'string *following-directory* *default-style-file*)
	   :direction :output
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

;TODO make exact location links to these togleable 
; (But need to index them first.)
(def-glist (notable (eql :notable)) list 
  (call-list list))

(def-glist :comment list)
(def-glist :note objects
  (wformat "(") (call-list objects) (wformat ")"))

(def-glist :series list
  (call-list list))

;;Lister here recognizes for styles (corresponding CSS.):
;;  none, circle, disc, square
;; Numberings:
;;  armenian, decimal, decimal-leading-zero, georgian, lower-alpha, 
;;  lower-greek, lower-latin, lower-roman, upper-alpha, 
;;  upper-latin, upper-roman

(def-glist (sep lister) list
  (unless (null list)
    (let ((style (slot-value sep 'gils::style)))
      (surround
	  (format nil (case style
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
  (call (glist-list (make-instance 'table)
	  (mapcar (lambda (el)
		    (b(car el)) (glist-list :series (cdr el)))
		  list))))

;;Links as actions and notes.

(defun sanitized-link (&rest rest)
  "TODO not very sturdy.."
  (substitute #\_ #\* (apply #'concatenate `(string ,@rest))))

(defun link-url (name &key (page (get-link name)))
  (typecase page
    (gil-info::link-entry
     (link-url name :page (gil-info::link-page page)))
    (page ;TODO recognize current page.
     (with-mod-slots "" (gil-info::directory) page
       (cond ;;NOTE _nasty_ if bugged!!!
	 ((and *cur-page* (eql (page-name *cur-page*) page))
	  (assert (string= (page-directory *cur-page*) directory) nil
		  "Eh why did the page directory change?")
	  (sanitized-link "#" (string name)))
	 (*cur-page*
	  (sanitized-link
	   (from-path-root directory (page-directory *cur-page*))
	   (string (page-name page)) ".html#" (string name)))
	 (t
	  (warn "Broken link! ~s" name)
	  (pushnew name *dead-links*)
	  (sanitized-link directory 
	    (string(page-name page)) ".html#" (string name))))))
    (gil-info::url-entry
     (slot-value page 'gil-info::url))
    (null ;Keep a list of failed links.
     (pushnew name *dead-links* :test #'equalp) 
     nil)
    (t
     (error "~a" page))))

(def-glist (link follow-link) objects
  (cond
    ((not (gils::name link))
     (call-list objects))
    ((link-url (gils::name link))
     (surround (format nil "a href=\"~a\"" (link-url (gils::name link)))
       (call-list objects)))
    (t
     (call (glist-list :underlined objects)))))

(def-glist (link link) objects
  (if (gils::name link) 
    (surround (sanitized-link "a name=\"" (string (gils::name link)) "\"")
      (call-list objects))
    (call-list objects)))

(def-surrounding-glist (url url-link)
    (format nil "a href=\"~a\"" (gils::name url)))

;;Basic modifiers.

(def-surrounding-glist :bold "b")
(def-surrounding-glist :italic "i")
(def-surrounding-glist :underline "u")
(def-surrounding-glist :underlined "u")
(def-surrounding-glist :strike "s")

(def-surrounding-glist :small "small")
(def-surrounding-glist :big "big")

(def-surrounding-glist :subscript "sub")
(def-surrounding-glist :superscript "sup")

(def-surrounding-glist :code "code")

(def-surrounding-glist :monospace "tt")

;Oh, yeah, html didnt preserve it here.. Kindah silly.
(def-glist :p-code objects
  (surround "code" ;Don't steal the whitespace.
    (surround "pre"
      (let ((*attempt-shorten* nil)) (call-list objects)))))

;;Headers & Sections.

(def-glist (header header) objects
  (surround (format nil "h~D" (clamp (slot-value header 'gils::level) 1 6))
    (call-list objects)))

(def-glist (section section) objects
  (denest
   (with-slots (gils::level gils::name gils::title) section)
   (cond
     ((> gils::level *section-page-level*)
      (call (link-pos gils::name
		      (header gils::level gils::title)))
      (call-list objects))
     (t
      (assert gils::name nil ;TODO get around it?
	      "To paginate a section, it _must_ have a name for html.")
      (let*((*cur-page* (get-page gils::name)))
	(with-open-file
	    (*standard-output*
	     (sanitized-link
	      (page-directory *cur-page*)
	      (format nil "~a" gils::name) ".html")
	     :direction :output
	     :if-exists :supersede :if-does-not-exist :create)
	  (surround "head" ;TODO better header making.
	    (surround "title" (call gils::title))
	    (style))
	  (let ((*indent-depth* 0))
	    (call (funcall *handle-page* 
			   gils::name 
			   (link-pos gils::name
				     (header gils::level gils::title))
			   objects)))))
      ;Return a notice of a linked section.
      (call (has-own-page gils::name))))))

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
  (assert *in-table* nil "Table elements _must_ be in a table!")
  (with-slots (gils::x-size gils::y-size
	       gils::x-align gils::y-align gils::x-span gils::y-span
	       gils::contents) table-el
    (let ((*in-table* nil))
      (surround
       (format nil "td~{ ~a~}"
	       (append
		(tagdata gils::x-size "width")
		(tagdata gils::y-size "height")
		(when-let (align
			   (case gils::x-align
			     ((:low :left) "left")
			     ((:high :right) "right")
			     ((:center) "center")
			     ((:justify) "center")))
		  (list-format "align=\"~a\"" align))
		(when-let (valign
			   (case gils::y-align
			     ((:low :bottom) "bottom")
			     ((:high :top) "top")
			     ((:center :middle) "middle")
			     ((:baseline) "baseline")))
		  (list-format "valign=\"~a\"" valign))
		(tagdata gils::x-span "colspan")
		(tagdata gils::y-span "rowspan")))
       (call-list gils::contents)))))

(def-glist (table table) elements
  (with-slots (gils::border gils::frame gils::rules gils::width
	       gils::cellpadding gils::cellspacing) table
    (let ((*in-table* t))
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

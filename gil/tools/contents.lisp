;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil-contents
  (:use :common-lisp :gil :gil-share :gil-style)
  (:export c-el c-el-seq use-contents)
  (:documentation "Tool to make contents from information contained in 
gil-info::*contents* after gil-executed with *lang* is :info.

Function gather-contents will do the scan :info does and provide input\
 for use-contents. c-el is various methods that give you ways to treat\
 the elements of the contents."))

(in-package :gil-contents)

(defun make-nbsp (cnt)
  (when cnt (glist-list :series (make-list cnt :initial-element :nbsp))))

;;Contents-el functions to be inputted into use-contents to specify how to
;; treat the elements.

(defgeneric c-el (name next &key)
  (:documentation "Contents-element. Helps in determining how to treat\
 elements when summarizing a contents page."))

(defmethod c-el (name next &key)
  (error "Content element not recognized: ~s" name))

(defparameter *c-el-keyargs* nil)

(defmacro def-c-el (name (&rest keyargs) &body body)
  "Defines contents-element maker. (no &key before keyargs needed 
Following are taken: function next, next, level, name, title, first"
  (let ((gname (gensym)))
    (setf *c-el-keyargs*
	  (remove-if (lambda (el)
		       (eql (car el) name)) *c-el-keyargs*))
    (assert
     (not(find-if (lambda (el) (eql (car el) name)) *c-el-keyargs*)))
    (prog1 
	`(defmethod c-el (,(if (symbolp name) `(,gname (eql ,name)) name)
			  next
			  &key ,@(cdr keyargs))
	   (declare (ignorable next))
	   (lambda (level name title &optional description)
	     (declare (ignorable level name first))
	     (macrolet ((next ()
			  '(funcall next level name title description)))
	       ,@body)))
      (push (cons name (copy-tree (cdr keyargs))) *c-el-keyargs*))))

(defun c-el-id (level name title &optional first)
  (declare (ignore level name first))
  title)

;TODO goddamn what is asdf's problem?
(def-c-el :id () ;This one does nothing.
  title)
(def-c-el :identity ()
  title)

(def-c-el :level-filter (&key (from 0) (to 2))
  "Filters out everything above some level. Use before anything else."
  (when (<= from level to) (next)))

(def-c-el :prep (&key (prep :bull) (prep-from 2) (prep-to 2))
  "Prepends something to element."
  (if (<= prep-from level prep-to)
    (series prep (next))
    (next)))

(def-c-el :header 
  (&key (give-level (lambda (level)
		      (values (+ level 1) (* 2 (- level 1))))))
  "Make contents with a series of various headers. 
give-level also provides a nonbreaking-space count."
  (multiple-value-bind (level nbsp-cnt) (funcall give-level level)
    (header (funcall give-level level)
	    (make-nbsp nbsp-cnt) (next))))

(def-c-el :nbsp
    (&key (nbsp-increment 3)
          (give-nbsp-cnt (lambda (level) (* nbsp-increment (- level 1)))))
  "Makes a contents page just by making newlines nonbreaking spaces."
  (series (make-nbsp (funcall give-nbsp-cnt level))
	  (next) :newline))

;;TODO table style?

(defvar *default-style-sizes*
  (vector 200 150 100 70 50 30 20 20 20 20 20 20 20))
(def-c-el :inline-style
    (&key (style-array *default-style-sizes*)
          (style (lambda (level)
		   (format nil "font-size:~a%" (aref style-array level)))))
  "Adds a style to contents element via inline sheet."
  (gil-style:inline-style (funcall style level) title))

(defun default-header-class-style
    (&key (style-array *default-style-sizes*) (l -1))
  (glist-list :series
    (map 'list (lambda (el)
		 (setq l (+ l 1))
		 (gil-style:register-style
		  (format nil "._cf~a{font-size:~a%}" l el)))
	 style-array)))

(def-c-el :class-style
    (&key (style (lambda (level)
		   (format nil "_cf~a" level))))
  "Adds a style to contents via references to classes. 
Defaultly named _cf~a with ~a filled with header level"
  (gil-style:refer-style (funcall style level)
    (next)))

(def-c-el :link ()
  (link name title))

(defmacro c-el-seq (&rest els)
  "Puts a bunch of c-el statements in sequence for you.
The last element needs to be a raw c-el output, or nil which does\
 #'c-el-id."
  (cond 
    ((null (cdr els))
     (if (or (symbolp (car els))
	     (and (listp (car els)) (keywordp (caar els))))
       `(c-el ,(car els) '#'c-el-id)
       (car els)))
    ((symbolp (car els))
     `(c-el ,(car els) (c-el-seq ,@(cdr els))))
    (t
     `(c-el ,(caar els) (c-el-seq ,@(cdr els)) ,@(cdar els)))))

#| TODO Not worky, infinite loops it seems..
 (defmacro def-c-el-seq (name (&rest override) &rest els)
  "Makes a composite c-el call, with all the arguments of each in it."
  (let ((els (if (stringp (car els)) (cdr els) els)))
  `(def-c-el ,name (&key ,@(copy-tree
			    (mapcan
			     (lambda (el)
			       (or (assoc (car el) override) el))
			     (mapcar
			      (lambda (el)
				(cdr (assoc el *c-el-keyargs*)))
			      els))))
     (c-el-seq
      ,@(mapcar
	 (lambda (el)
	   (cons el (mapcan
		     (lambda (kv)
		       `(,(intern (symbol-name (car kv)) :keyword)
			  ,(car kv)))
		     (cdr (assoc el *c-el-keyargs*)))))
	 els)
      (next)))))

;;Default composit contents elements. 
;(def-c-el-seq :prepped-bsp-based ()
;  :level-filter :nbsp :class-style :prep)

;(def-c-el-seq :prepped-header-based ()
;  :level-filter :nbsp :header :prep) |#

(defun use-contents
    (contents &optional
     (treatment (c-el-seq :class-style :nbsp :prep :link)))
  "Produces contents page with element treated based treatment."
  (glist-list :series
    (mapcar (lambda (section)
	      (with-slots
		    (gils::level gils::name gils::title gils::description) 
		  section
		(funcall treatment gils::level gils::name gils::title
			 gils::description)))
	    contents)))

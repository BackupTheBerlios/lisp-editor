;;
;;  Copyright (C) 2010-03-01 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil-info
  (:use :common-lisp :generic :denest :gil :gil-share :gil-read)
  (:export use-contents gather
	   link-to-url)
  (:documentation
   "Contains gathering data for contents, and creation of contents section.
Gathering contents also registers and handles links if not already\
 registered."))

(in-package :gil-info)

(defvar *contents* nil)
(defvar *links* (make-hash-table))
(defclass link-entry ()
  ((page :initarg :page)))
(defun find-link-page (link-name)
  (gethash (gils::intern* link-name) *links*))

(defvar *register-first* nil)

(def-call (fun function) (funcall fun))
(def-call (str string) str)
(def-call (num number) num)
(def-call anything (error "Not recognized ~a" anything))
(def-call (null null) (declare (ignore null)))

(defun gather (from)
  "Collects table of contents from files."
  (unless (eql *lang* :info)
    (let ((*lang* :info)
	  (gils::*handle-page* #'identity))
      (call (gil-read::execute from)))
    (setq *contents* (reverse *contents*))))

(defun register-link (link-name)
  (let ((link-sym (gils::intern* link-name)))
    (typecase (gethash link-sym *links*)
      (link-entry
       (warn "Position that can be linked to created twice. 
Links might go not as intended! Link name: ~a
Page ~a changed to page ~a." link-sym
        (slot-value (gethash link-sym *links*) 'page) gils::*cur-page*)))
    (setf (gethash link-sym *links*) 
	  (make-instance 'link-entry :page gils::*cur-page*))))

(def-glist anything objects
  (declare (ignore anything))
  (mapcar #'call objects))

(def-glist (link gils::link) objects
  (register-link (gils::name link))
  (mapcar #'call objects))

(def-glist (link gils::follow-link) objects
  (declare (ignore link))
;  (register-link (gils::name link))
  (mapcar #'call objects))

(def-glist (url-link gils::url-link) objects
  (declare (ignore url-link))
  (mapcar #'call objects))

(def-glist (section section) list
  (with-slots (gils::level gils::name gils::title) section
    ;TODO in big doodoo, i need title for _different language_
    (push (list gils::level gils::name gils::title 
		(when *register-first* (car list)))
	  *contents*)
    (cond
      ((> gils::level gils::*section-page-level*)
       (register-link gils::name)
       (call gils::title)
       (mapcar #'call list))
      (t ;Produces new page.
       (let ((gils::*cur-page*; (gils::intern* gils::name))
	      (if-let path (gethash gils::name gils::*page-path*)
	        (format nil "~a~a" path gils::name) ;Path specified.
		gils::name)))
	 (register-link gils::name)
	 (call gils::title)
	 (mapcar #'call list))))))

(def-call (table-el table-el)
  (call-list (slot-value table-el 'gils::contents)))

(def-glist (table gils::table) contents
  (mapcar (lambda (el)
	    (if (listp el)
		(mapcar #'call el)
		(call el)))
	  contents))

;;TODO seems like a general-purpose function to me..
(defun rank (list &key (compare #'<=))
  "Makes a hierarchical tree of the elements preserving order, based on\
 comparing."
  (when (null list)
    (return-from rank nil))
  (gen:if-let i (position-if (gen:curry compare (car list)) (cdr list))
    (cons (cons (car list)
		(rank (subseq (cdr list) 0 i) :compare compare))
	  (rank (subseq (cdr list) i) :compare compare))
    (list (cons (car list) (rank (cdr list) :compare compare)))))
  
(defun organized-contents (contents)
  "Organizes contents."
  (unless *contents*
    (warn "Trying to use contents when no elements."))
  (rank contents :compare (lambda (a b)
			      (<= (car a) (car b)))))

;;Use data to make contents page.

;TODO it makes a mess of it..
(defun ranked-list (list &key number (number-upto (if number 4 0))
		    (include-upto 3)
		    (via :header) replace-names
		    (index (list)) (header-add 2))
  "Makes a list with multiple levels for a list that went through RANK,
the elements must be: (level link-name object &optional first-obj)"
  (let ((via (if (listp via) via (list via)))
	(si -1)) ;Sub-list: TODO feel precarious..
    (flet ((rl (list &key (index index) (via via))
	     (ranked-list list :include-upto include-upto
			  :number-upto number-upto :via via
			  :replace-names replace-names
			  :index index :header-add header-add))
	   (do-el (el)
	     (destructuring-bind
		   (level link-name title &optional first-obj) el
	       (declare (ignore first-obj))
	       (when (<= level include-upto)
		 (let*((obj ;Add number if numbering.
			(if (<= level number-upto) 
			  (glist :series
				 (prep
				  (format nil "~{~a~^.~} " 
					  (reverse (copy-list index))))
				 title)
			  title))
		       (link-name (if-use ;Add link if link specified.
				   (assoc link-name replace-names
					  :test #'string=)
				   link-name))
		       (title-obj
			(if link-name
			  (link link-name obj) obj)))
		   (case (car via)
		     (:header (list (header (+ level header-add) 
					    title-obj)))
		     (t       (list title-obj))))))))
      ;Single sub-elements not worth giving index.
      (unless (find-if-not (lambda (el) (listp(car el))) list)
	(return-from ranked-list 
	  (glist-list :series (mapcar #'rl list))))
      (glist-list (case (car via) (:header :series) (t (car via)))
	 (mapcan (lambda (el)
		   (cond
		     ((listp (car el)) ;TODO don't drag in first one..
		      (setf- + si 1)
		      (list(rl el :index (cons si index)
			       :via (if-use (cdr via) via))))
		     (t
		      (when index (setf- + (car index) 1))
		      (do-el el))))
		 list)))))

(defclass url-entry ()
  ((url :initarg :url :type string)))
(defun link-to-url (link-name url)
  "Links a link-name to an (arbitrary)url."
  (setf (gethash (gils::intern* link-name) *links*)
	(make-instance 'url-entry :url url)))

(defun use-contents
    (contents &key number (number-upto (if number 4 0)) (include-upto 3)
     (via :header) replace-names
     (index (list 0)))
  "Reads the (gil) files, organizing these hierarchically, in order to\
 produce a contents list.
Note that for via other than :header, it currently makes a bit of a mess."
  (unless (eql *lang* :info)
    (ranked-list (organized-contents contents) :include-upto include-upto
		 :number number :number-upto number-upto :via via
		 :replace-names replace-names :index index)))

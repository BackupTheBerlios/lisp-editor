
(defpackage mk-website
  (:use :common-lisp :denest :generic :lml2)
  (:export with-bars mk-website
	   mk-website-from-code mk-website-from-file)
  (:documentation "Website making tools for lml2"))

(in-package :mk-website)

(defun with-bars (table-args body
		  &key top  bottom left right
		       top-a bottom-a left-a right-a
		       frame-file)
  "Allows one to add bars on top, bottom, left and right."
  (flet ((add (tp body &optional attribute)
	   (cond
	     (attribute
	      `(((,tp ,@attribute) ,@body)))
	     (body
	      `((,tp ,@body))))))
    `(,(if table-args `(:table ,@table-args) :table)
       (:tbody ,@(add :tr top top-a)
	       (:tr 
		,@(add :td left left-a)
		,@(add :td body)
		,@(add :td right right-a))
	       ,@(add :tr bottom bottom-a)))))

(defun mk-website (&key page-list
		   (manner :table) left left-a; '((:valign :top)))
		   top top-a bottom bottom-a right right-a
		   single-page single-page-header
		   table-args)
  "Makes a simple website.
Page list is regular lml2 file, with filename at start.
 The head is stripped off if you decide to \
 make it a single page. Single-page-header is then the header.
Make contents file yourself, you can put it by putting regular lml2 code\
 in left. I suggest (:valign :top) for left-a. (Default.)"
  (flet ((wbars (body) ;TODO use title in headers?
	   (with-bars
	     table-args body :left left
	     :left-a left-a :top top :top-a top-a
	     :bottom bottom :bottom-a bottom-a
	     :right right :right-a right-a))
	 (strip-head (p)
	   "Remove the header, if single-page"
	   (cond
	     ((not(listp (cadr p)))
	      (cdr p))
	     ((eql (caadr p) :head)
	      (cddr p))
	     (t
	      (cdr p)))))
    (cond
      ((or single-page single-page-header)
       (eval `(html-file-page (,single-page)
		,single-page-header
		(:body
		 ,(wbars (denest (collecting ())
				 (dolist (p page-list)
				   (appending (strip-head p)))))))))
      (t
       (dolist (p page-list)
	 (when (car p) ;If not, only meant for single-page.
	   (eval `(html-file-page (,(car p))
		    (:body
		     ,@(when (when (listp (cadr p))
			       (eql (caadr p) :head))
			     (list (cadr p)))
		     ,(wbars (strip-head p)))))))))))

(defun mk-website-from-file (file)
  "Makes a website from a file format. See mk-website-from-code for more"
  (mk-website-from-code
   (with-open-file (stream file)
     (do ((read (read stream nil nil) (read stream nil nil))
	  (code (cons read code) (cons read code)))
	 ((null read) (reverse code))))))
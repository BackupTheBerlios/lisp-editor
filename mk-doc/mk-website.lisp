
(defpackage mk-website
  (:use :common-lisp :iterate :generic :lml2)
  (:export with-bars mk-website)
  (:documentation "Website making tools for lml2"))

(in-package :mk-website)

(defun with-bars (table-args body
		  &key top  bottom left right
		       top-a bottom-a left-a right-a (manner :table))
  "Allows one to add bars on top, bottom, left and right. "
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

(defun mk-website (&key contents page-list
		   (manner :table) one-page contents-a one-page-header
		   top top-a bottom bottom-a right right-a
		   table-args)
  "Makes a simple website."
  (flet ((wbars (body)
	   (with-bars
	     table-args body :left contents :manner manner
	     :left-a `(:valign :top ,@contents-a) :top top :top-a top-a
	     :bottom bottom :bottom-a bottom-a
	     :right right :right-a right-a))
	 (strip-head (p)
	   (cond
	     ((not(listp (cadr p)))
	      (cdr p))
	     ((eql (caadr p) :head)
	      (cddr p))
	     (t
	      (cdr p)))))
    (cond
      (one-page
       (eval `(html-file-page (,one-page)
		,one-page-header
		(:body
		 ,(wbars (iter (for p in page-list)
			       (appending (strip-head p))))))))
      (t
       (dolist (p page-list)
	 (eval `(html-file-page (,(car p))
		   (:body
		    ,@(when (when (listp (cadr p))
			      (eql (caadr p) :head))
		        (list (cadr p)))
		    ,(wbars (strip-head p))))))))))
			

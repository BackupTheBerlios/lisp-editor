;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl)

(defpackage :lml2-to-gil
  (:use :common-lisp :alexandria :generic :denest
	:gil :gil-share :gil-style)
  (:export )
  (:documentation "Initial development

Is to make lml2 from gil. If a semi-inverse is ever made,\
 it should be in gil/output."))

(in-package :lml2-to-gil)

(defun lml2-to-gil (lml2 &key list-style)
  (denest 
   (destructuring-bind (tag &rest body) lml2)
   (flet ((do-body (body)
	    (mapcar #'lml2-to-gil body))
	  (here (sym &optional (body-from body))
	    `(,sym ,@(do-body body-from)))))
   (cond
     ((listp tag) ;TODO look at styles.
      (destructuring-bind (tag-name &key style class href &rest stuff) tag
	(case tag-name
	  ((:ol :ul)
	  (t
	   `(denest
	     ,@(when style 
	         `((inline-style ,style)))
	     ,@(when class
	         `((refer-style ,class)))
	     ,@(when (and (eql tag-name :a) href) ;TODO problematic.. 
		 `((url-link href)))              ; how to 'internalize' links
;	  ,@(when (and (eql tag-name :name) href)
;	      `((url-link href)))
	     ,(lml2-to-gil ,tag-name ,@body))))))
     (t
      (case tag
	(:a
	 (error "Didn't expect these loose!! Though i saw a purpose for\
 it, but currently unimplemented. TODO"))
;; :em :strong :dfn :samp :kbd :var
	((:b :i :u :strike :small :big :subscript :superscript
	  :code :comment :cite) ;;TODO add those not here yet.
	 (here (intern (symbol-name tag))))
	(:tt
	 (here 'monospace))
	(:blockquote
	 (here 'p-quotation))
	(:q
	 (here 'quotation))
	(:p
	 (case (when (null (cdr body)) (caar body))
	   (:code (here 'p-code (cdar body)))
	   (:q    (here 'p-quotation (cdar body)))
	   (t     (here 'p (cdar body)))))
	((:ul :ol)
	 `(,@(if list-style
	       `(lister ,list-style )
	       (list (case (car lml2)
		       (:ul 'point-list) (:ol 'numbered-list))))
	     ,@(mapcar (lambda (el)
			 `(series ,@(typecase el
				      ((cons (eql :li)) (body (cdr el)))
				      (list             el)
				      (t                (list el)))))
		       (cdr lml2))))
	(:dl
	 `(description-list 
	   ,@(mapcar (lambda (el)
		       (destructuring-bind
			     ((dt &rest dt-body) (dd &rest dd-body)) el
			 (assert (and (eql dt :dt) (eql dd :dd)))
			 `(list (series ,@dt-body) (series ,@dd-body))))
		     body)))
	(:br
	 :newline)
	(:hr
	 tag)
	   

;;Deprecated, mostly style stuff.
;  :basefont
;  :center
;  

(html-parse:parse-html "<!--> kakad-->")

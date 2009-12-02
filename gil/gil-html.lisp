
(cl:in-package :cl)

(defpackage gil-html
  (:use :common-lisp :generic :gil :gil-share))

(in-package :gil-html)

(defvar *section-page-level* 1)

(defun default-style (&optional (file "default"))
  (format nil "<head>
<link rel=\"stylesheet\" type=\"text/css\" href=\"~D.css\" />
</head>" file))

(make-gil-definer :html def-gil-method def-gil-method*)

(defun surround (with fill)
  (wformat "<~D>" with)
  (funcall fill)
  (wformat "</~D>" (subseq with 0 (position #\Space with))))

;;-----------------------Prep objects---------------------------------------

(def-gil-method* i-prep (string string) ()
  (wformat string))

(def-gil-method i-prep (string t) ()
  string);(wformat string))

;;-----------------------List-like------------------------------------------
(def-gil-method* i-glist :p ((list list))
  (wformat "<p>")
  (dolist (el list)
    (funcall el) (write-char #\Newline))
  (wformat "~%</p>~%"))

(def-gil-method* i-glist :series ((list list))
  (dolist (el list)
    (funcall el)))

(def-gil-method i-glist (style symbol) ((list list))
  (i-glist :html (mk dot-list :style style) list))


;;Lister here recognizes for styles (corresponding CSS.):
;;  none, circle, disc, square
;; Numberings:
;;  armenian, decimal, decimal-leading-zero, georgian, lower-alpha, 
;;  lower-greek, lower-latin, lower-roman, upper-alpha, 
;;  upper-latin, upper-roman

(def-gil-method* i-glist (sep dot-list) ((list list))
  (let ((style (dot-list-style sep)))
    (surround (format nil (case (case style
				  (:numbered-list :decimal)
				  (:list          :disc)
				  (:alt-list      :square))
			    ((:armenian :decimal :decimal-leading-zero 
			      :georgian :lower-alpha :lower-greek
			      :lower-latin :lower-roman :upper-alpha
			      :upper-latin :upper-roman)
			       "ol class={\"~D\"}")
			    (t "ul class={\"~D\"}"))
		      (if (symbolp style)
			  (symbol-name (dot-list-style sep)) "DISC"))
	      (lambda ()
		(dolist (el list)
		  (surround "li" el)
		  (write-char #\Newline))))))

;;--------------------Links as actions and notes----------------------------

(def-gil-method* i-action (link link) (object)
  (with-access (gils:link-name gils:link-page) link
    (surround (format nil "a href=\"~D~D~D\""
		      (if-use gils:link-page "")
		      (if gils:link-name "#" "")
		      (if-use gils:link-name ""))
	      object)))

(def-gil-method* i-note (note link) (object)
  (surround (format nil "a name=\"~D\"" (link-name note)) object))

;;-------------------Basic modifiers----------------------------------------

(def-gil-method* i-note :bold (object)
  (surround "b" object))
(def-gil-method* i-note :italic (object)
  (surround "i" object))
(def-gil-method* i-note :underlined (object)
  (surround "u" object))

;;-------------------Headers & Sections-------------------------------------

(def-gil-method* i-header (level integer) (object)
  (surround (format nil "h~D" (clamp level 1 6)) object))

#| TODO make headers make pages.
 (def-gil-method i-section
    (level integer) ((name string) object (paragraphs list))
  (flet ((result ()
	   (glist :p (cons (header level object) paragraphs))))
    (if (> level *section-page-level*) (result)
      (with-open-file
	  (stream name :if-exists :supersede :if-does-not-exit :create)
	(pushnew name *pages*)
	(format stream (let ((*cur-page* name))
			 (result)))
	""))))
|#

;;Images.

(def-gil-method* i-prep (image file-image) ()
  (wformat "<img src=\"~D\" \>" (file-name image)))

(cl:in-package :cl-user)

(defpackage gil-html
  (:use :common-lisp :generic :gil :gil-share))

(in-package :gil-html)

(defvar *section-page-level* 1)

(defvar *default-style-file* "default")

(defun style (&optional (file *default-style-file*))
  (wformat "<link rel=\"stylesheet\" type=\"text/css\" href=\"~D.css\"\
 />~%"
	   file))

(make-gil-definer :html def-gil-method def-gil-method*)

(defun surround (with fill)
  (wformat "<~D>" with)
  (funcall fill)
  (wformat "</~D>" (subseq with 0 (position #\Space with))))

;;-----------------------Prep objects---------------------------------------

(def-gil-method i-prep (fn function) ()
  fn)
(def-gil-method* i-prep (thing t) ()
  thing)

(def-gil-method* i-prep (string string) ()
  (wformat string))

(def-gil-method* i-prep (number number) ()
  (wformat "~D" number))

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
	      (lambda ()
		(dolist (el list)
		  (surround "li" el)
		  (write-char #\Newline))))))

;;--------------------Links as actions and notes----------------------------

(def-gil-method* i-action (link link) (object)
    (let ((name (gils::name link))
	  (page (find-page name)))
      (surround (if page
		  (format nil "a href=\"~a.html#~a\"" page name)
		  (format nil "a href=\"~a.html\"" gils::name))
		object)))

(def-gil-method* i-note (note link) (object)
  (surround (format nil "a name=\"~a\"" (gils::name note)) object))

(def-gil-method* i-action (url url-link) (object)
  (surround (format nil "a href=\"~a\"" (gils::name url)) object))

;;-------------------Comments----------------------------------------------
(def-gil-method* i-note :comment (object))

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

(def-gil-method i-section
    (level integer) (name object (paragraphs list))
  (flet ((result ()
	   (apply #'glist (append (list :p (header level object))
				  paragraphs))))
    (if (> level *section-page-level*)
      (link-pos (result) name) ;Note: link wraps round whole thing.
      (with-open-file
	  (*standard-output* (format nil "~D.html" name) :direction :output
		  :if-exists :supersede :if-does-not-exist :create)
	(let ((*cur-page* (gils::get-page (if (stringp name) (intern name)
					      name))))
	  (wformat "<head>~%") ;TODO make a note for header-making.
	  (surround "title" object)
	  (style)
	  (wformat "</head>")
	  (funcall (result)))
	;Return a notice of a linked section.
	(p(link (format nil "-> section ~D has own page" name)
		name *cur-page*))))))

;;Images.
(def-gil-method* i-prep (image base-image) ()
  (warn "Html output(at least this version) doesn't know what to do with\
 image type ~D."
	(type-of image)))

(def-gil-method* i-prep (image file-image) ()
  (wformat "<img src=\"~D\" \>" (file-name image)))

;;-------------------'Windows'---------------------------------------------

(def-gil-method* i-glist (split gils::split) ((frames list))
  (i-glist :check split frames)
  (with-slots (gils::spacing gils::way gils::dir) split
    (flet ((frames ()
	     (mapcar
	      (lambda (frame pos)
		(surround ;TODO do with styles.
		 (format nil "td ~s=~a"
			 (case gils::dir
			   (:v "width") (:h "height"))
			 (case gils::way
			   (:relative
			    (format nil "~a%" (round (* 100 pos))))
			   (:absolute
			    pos)))
		 frame)
		(wformat "~%"))
	      frames gils::spacing)))
      (surround "table"
		(case gils::dir
		  (:v (lambda () (surround "tr" #'frames)))
		  (:h #'frames))))))

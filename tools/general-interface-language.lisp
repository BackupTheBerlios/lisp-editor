(cl:in-package :cl)

(defpackage :gil
  (:use :common-lisp :generic)
  (:export mk *lang*
	   *list-step* *list-dot-style* *list-index* *list-deepen*
	   list-step list-dot-style list-index list-deepen
	   paragraphs point-list glist
	   action action* note note*
	   *link-page-style* link-page-style link link* link-pos link-pos*
	   header header* section
	   i-header i-section i-glist i-action i-note)
  (:documentation
   "Attempt at a general interface and even markup language.

Note that this package does not implement any implementation! 
It only defines functions for it.
Note that you will want to use DENEST with this!"))

(in-package :gil)

(defmacro changer (changer-name var-name
	           &key (doc "Changes a variable, see the variable doc."))
  "Makes a variable changer for a given variable."
  `(defmacro ,changer-name (,var-name &body body)
     ,doc
     (append (list 'let (list (list var-name ,var-name)))
	     body)))

(defmacro convenience (name &rest args)
  `(defmacro ,name (,@(butlast args) &rest ,(car(last args)))
     "Convenience macro for the non-* version.\
 The last argument is &rest-ed."
     (let ((fun-name (symbol-name name)))
	     
       (cons (intern (subseq fun-name 0 (- (length fun-name) 1)))
	     args))))

(defmethod mk (type &rest args)
  `(make-instance ',type ,@args))

(defvar *lang* nil "Current language to convert too.")
(changer lang *lang*)

;;-------------------List-like objects--------------------------------------

(defvar *list-step* nil "Steps listers make.")
(changer list-step *list-step*)

(defvar *list-dot-style* nil "Style of the dots or indexing.")
(changer list-step *list-dot-style*)
(defvar *list-index* 0 "Index of listing when it indexes.")
(changer list-index *list-index*)

(defvar *list-deepen* t "Whether the next call of a lister goes to a next\
 depth of lists. Defaultly on and defaulty glist turns it off.")
(changer list-deepen *list-deepen*)

(defun paragraphs (lang &rest paragraphs)
  "List of paragraphs. (glist with :p)"
  (glist lang :p paragraphs))

(defun point-list (lang &rest paragraphs)
  "List of points. (glist with "
  (glist lang :list paragraphs))

(defun glist (sep &rest things)
   "Lists of various forms.
sep:
 :p    Paragraph-like separations
 :list Point-by-point list, class point-list allows for more specification.
 If you made a custom one, and none applies, it reverts to :p"
  (i-glist *lang* sep things))

(defgeneric i-glist (lang sep things)
  (:documentation "See glist."))

(defmethod i-glist (lang sep (things list))
  (i-glist lang :p things))

;;-------------------Add actions to objects---------------------------------
(defun action (action object)
  "Adds action to an object."
  (i-link *lang* action object))

(convenience action action object)

(defgeneric i-action (lang action object)
  (:documentation "Adds posibility of action to object."))

;;-------------------Add anotations to objects------------------------------
(defun note (note object)
  (i-note *lang* note object))

(convenience note note object)

(defgeneric i-note (lang note object)
  (:documenation "Adds an annotation to an object."))

;;-------------------Link-like; action to follow link-----------------------

(defclass link ()
  (name) (:documentation "Link to a position."))

(defvar *link-page-style* nil
  "Way the link is followed with regard to page, may try to open new tab or\
 replace old, etcetera..")
(changer link-page-style *link-follow-style*)

(defun link (link object)
  "Adds action to link to an object."
  (action (mk link :name link) object))

(convenience link link object)

;;And link-positions; annotations that links can go there by some name.
(defun link-pos (name object)
  "Adds a notation that links can go here."
  (note (mk link :name name) object))

(convenience link-pos name object)

;;-------------------Sections and headers-----------------------------------
(defun header (level object)
  "A title of a paragraph/other."
  (i-header *lang* level object))

(convenience header level object)

(defgeneric i-header (lang level object)
  (:documentation "A title of a paragraph/other."))

(defun section (level object &rest paragraphs)
  (i-section *lang* level object paragraphs))

(defgeneric i-section (lang level object paragraphs)
  (:documentation "A titled set of paragraphs, defaultly combines header\
 and paragraph itself."))

(defmethod i-section (lang level object (paragraphs list))
  (glist :p (cons (header level object) paragraphs)))

;;Some declaims.
(declaim (inline paragraphs glist action note link link-pos))


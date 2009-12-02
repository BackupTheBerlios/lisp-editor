;;Author: Jasper den Ouden

(cl:in-package :cl)

(defpackage :gil
  (:use :common-lisp :generic)
  (:export mk *lang* rest-version
	   prep glist action action* note note*
	   header header* section
	   i-prep i-header i-section i-glist i-action i-note)
  (:documentation
   "GIL: General Interface _Library_ (But be sure to confuse people with 
the L being Language ;) )

Note that this package does not implement any implementation! 
It only defines functions for it.
Note that you will want to use DENEST with this!

A lot of the defvars are some things implementors can hang on to,
so they're applicable to multiple implementations."))

(in-package :gil)

(defmacro rest-version (name &rest args)
  `(defmacro ,(intern (format nil "~D*" (symbol-name name)))
       (,@(butlast args) &rest ,(car(last args)))
     "Convenience macro for the non-* version.\
 The last argument is &rest-ed."
     (cons ',name (list ,@args))))

(defmacro mk (type &rest args)
  "Macro to shorten up make-instance."
  `(make-instance ',type ,@args))

(def-changable-var *lang* :init nil :doc "Current language to convert too.")

(def-changable-var *cur-page* :init nil)
(def-changable-var *pages* :init nil)

;;-------------------Preprocessing objects----------------------------------
(defgeneric i-prep (lang thing)
  (:documentation "Preprocess object."))

(defmethod i-prep (lang thing)
  thing)

(defun prep (thing)
  (i-prep *lang* thing))

;;-------------------List-like objects--------------------------------------

(defgeneric i-glist (lang sep things)
  (:documentation "See glist."))

(defun glist (sep &rest things)
  "Lists of various forms.
sep:
 :p    Paragraph-like separations
 :list Point-by-point list, class point-list allows for more specification.
 If you made a custom one, and none applies, it reverts to :p"
  (i-glist *lang* sep (mapcar #'prep things)))

;;-------------------Add actions to objects---------------------------------

(defgeneric i-action (lang action object)
  (:documentation "Adds posibility of action to object."))

(defun action (action object)
  "Adds action to an object. 
Like following a link, as link does."
  (i-action *lang* (prep action) (prep object)))

(rest-version action action object)

;;-------------------Add anotations to objects------------------------------

(defgeneric i-note (lang note object)
  (:documentation "Adds an annotation to an object."))

(defun note (note &rest objects)
  "Adds an annotation to an object.
For instance, as link-pos makes as in gil-suggest, but meant to be more\
 general."
  (i-note *lang* (prep note) (glist :series objects)))

(rest-version note note object)

;;-------------------Sections and headers-----------------------------------

(defgeneric i-header (lang level object)
  (:documentation "A title of a paragraph/other."))

(defun header (level object)
  "A title of a paragraph/other."
  (i-header *lang* (prep level) (prep object)))

(rest-version header level object)

(defgeneric i-section (lang level name object paragraphs)
  (:documentation "A titled set of paragraphs, defaultly combines header\
 and paragraph itself."))

;Default section behavior.
(defmethod i-section (lang level name object (paragraphs list))
  (declare (ignore name))
  (i-glist lang :p (cons (header (prep level) (prep object))
			 (mapcar #'prep paragraphs))))

(defun section (level name object &rest paragraphs)
  (i-section *lang* (prep level) name (prep object)
	     (mapcar #'prep paragraphs)))

;;-----------------TODO spatial distribution.-------------------------------


;;Some declaims.
(declaim (inline glist action note))

;;
;;  Copyright (C) 2010-03-01 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :exemplary
  (:use :common-lisp :generic :denest
	:gtk-stuff :example)
  (:shadowing-import-from :example
			  type name examples args arg-infos body fun)
  (:export gtk-example)
  (:documentation "TODO there could be infinite recursion if only one \
 possibility in circles."))

(in-package :exemplary)

;;TODO make the bits that shouldn't expand NOT expand.

(defclass gtk-example (gtk:notebook)
  ((arg-widgets :initform nil :type list)
   (example :type fun-example :initarg :example
     :documentation "The underlying class."))
  (:metaclass gtk:gobject-class)
  (:documentation "Interface based on examples for arguments into functions,
 possibly mirroring normal use of functions."))

(defgeneric mk-arg-example (arg-info example &key)
  (:documentation "Makes an interface for a single example-argument."))

(defgeneric arg-example-name (example)
  (:documentation "Name of example."))

(defmethod arg-example-name ((example fun-example))
  (string-downcase (symbol-name (slot-value example 'name))))

(defmethod access ((example gtk-example) (arg symbol))
  (with-slots (arg-widgets) example
    (let ((widget
	   (widget (slot-value (getf arg-widgets arg) 'cw))))
      (access widget :value))))

(defmethod arg-example-name ((example list))
  (string-downcase (symbol-name (car example))))

;List-indicated examples.
(defmethod mk-arg-example ((arg-info arg-info) (example list) &key short)
  (declare (type boolean short))
  (case (car example)
    (:string ;Short string with possibly setter.
     (destructuring-bind (&key set (set-fn (when set (lambda () set)))
			       (text (if set-fn (funcall set-fn) "")))
	 (cdr example)
       (if set-fn (make-instance 'entry-with-setter
		    :set-fn set-fn :text text)
	          (make-instance 'gtk:entry :text text))))
#|    (string-choice ;Seems to indicate i have to improve selector.
     (destructuring-bind (initial &rest choices) (cdr example)
       (make-instance 'selector :initial initial :no-signal t
	:choice-list (mapcar (lambda (c)
			       (list c (make-instance 'gtk:label :label c
						      :relief :none)
				     (make-instance 'gtk:frame)))
		       :widget (make-instance 'gtk:label
				 :label (format nil "c"
						    :relief :none)))))
|#
;    (:text
    (:boolean
     (make-instance 'gtk:toggle-button
       :label (string-downcase (symbol-name (slot-value arg-info 'name)))))
    (:number
     (destructuring-bind (&key min max) (cdr example)
       (let ((adjustment (make-instance 'gtk:adjustment
			   :lower min :upper max)))
	 (if short (make-instance 'gtk:spin-button :adjustment adjustment)
	           (make-instance 'linear number-maker
				  :adjustment adjustment)))))
    (:font
     (if short (make-instance 'gtk:font-button)
	       (make-instance 'gtk:font-selection)))
    (:date
     (make-instance 'gtk:calendar))
    (:color
     (if short (make-instance 'gtk:color-button) 
	       (make-instance 'gtk:color-selection)))
    (:file ;TODO make editing box a little more powerful if possible.
     (if short (make-instance 'gtk:file-chooser-button)
	       (make-instance 'gtk:file-chooser-widget)))
    (:code ;TODO needs to become a proper editor, make new package for it.
     ;TODO eh it errors!?
;     (make-instance 'gtk:editable :editable t))))
     )))

;fun-example based.
(defmethod mk-arg-example ((arg-info arg-info) (example fun-example) &key)
  (declare (type boolean short))
  (let ((interface (make-instance 'gtk-example :example example)))
    interface))

(defvar *selector-xalign* 0.0 "x-alignment of entries of selector.")

(defun example-choice (arg-info)
  "Makes an entry for the selector which is used for selecting from \
examples"
  (lambda (example &key (name (arg-example-name example)))
    (list name
	  (make-instance 'gtk:button
	    :label name :relief :none :xalign *selector-xalign*)
	  :widget-fn (let (memoized)
		       (lambda ()
			 (if-use memoized
			   (setq memoized
				 (mk-arg-example arg-info example))))))))

(defun arg-input-maker (arg-info)
  "Makes an example-maker."
  (with-slots (type name examples) arg-info
    (make-instance 'frame-selector
      :choice-list (mapcar (example-choice arg-info) examples))))

(defmethod initialize-instance :after
    ((ex gtk-example) &key (dir :v) (size 5)
     (a-lot 10) list-ize-key ;Putting arguments through a selector.
     (selector-xalign *selector-xalign*))
  (denest
   (with-slots (arg-widgets example) ex)
   (with-slots (arg-infos) example)
   (labels ((tab-name (symbol)
	      (string-downcase (if (symbolp symbol)
				 (symbol-name symbol) symbol)))
	    (tab-label (symbol)
	      (make-instance 'gtk:label	:label (tab-name symbol)))))
   (let ((k 0)
	 (*selector-xalign* selector-xalign)))
   (collecting (nil rest-choices)
     (dolist (arg-info arg-infos)
       (setf- + k 1) ;Keep count.
       (with-slots (type name) arg-info
	 (setf (getf arg-widgets name) (arg-input-maker arg-info))
	;Set the plist of argument widgets.
	 (cond
	   ((and (< k a-lot) ;Unless in the selector, they go into tabs.
		 (not (and list-ize-key (eql type '&key))))
	    (gtk:notebook-add-page ex (getf arg-widgets name)
				   (tab-label name)))
	   (t ;Collect, in order to put into selector.
	    (collecting
		(list (tab-name name)
		      (make-instance 'gtk:button :name (tab-name name)
		         :relief :none :xalign *selector-xalign*)
		      :widget (getf arg-widgets name)))))))
     (cond
       ((null rest-choices)) ;Nothing in selector, don't need tab.
       ((null (cdr rest-choices)) ;If single in selector, just make it tab.
	(gtk:notebook-add-page ex (caadr rest-choices)
	  (tab-label (caar rest-choices))))
       (t ;Make tab with a selector of the remaining arguments.
	(gtk:notebook-add-page ex
	  (make-instance 'frame-selector :choice-list rest-choices)
	  (tab-label "Other"))))))
  ex)

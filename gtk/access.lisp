(in-package :cl)
(in-package :gtk-stuff)

(defgeneric access (example argument)
  (:documentation "Access input for example."))

(defmethod access (thing (by null))
  thing)
(defmethod access (thing (by list)) ;note method above!
  (access (access thing (car by)) (cdr by)))

(defmacro value-access ((w &optional (by :value)) &body access-bodies)
  `(progn
     ,@(mapcar (lambda (b)
		 `(defmethod access ((,w ,(car b)) (by (eql ,by)))
		    ,@(cdr b)))
	       access-bodies)))

(value-access (w)
  (gtk:entry
   (gtk:entry-text w))
  (entry-with-setter
   (gtk:entry-text (slot-value w 'entry)))
  (gtk:toggle-button
   (gtk:toggle-button-active w))
;  (gtk:font-button
  (gtk:spin-button
   (gtk:spin-button-value w))
  (gtk:color-selection
   (gtk:color-selection-current-color w))
  (gtk:color-button
   (gtk:color-button-color w)))
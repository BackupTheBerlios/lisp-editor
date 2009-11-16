(in-package :cl)

(defpackage :gtk-stuff
  (:use :common-lisp :generic)
  (:export
   list-frame list-window changing-widget changer-tab entry-with-setter
   widget))

(in-package :gtk-stuff)

(defgeneric widget (obj)
  (:documentation "Getting widget within an object."))
(defgeneric (setf widget) (to obj)
  (:documentation "Settting a widget within an object,\
 possibly changing things such that it is shown."))

(defclass list-frame (gtk:frame)
  ((top-box :accessor box))
  (:documentation
   "A frame that can be directly filled with a list of stuff..")
  (:metaclass gtk:gobject-class))

(defun make-box (dir)
  "Keyword to h-box and v-box instances."
  (make-instance (case dir (:v 'gtk:v-box) (:h 'gtk:h-box) (t  dir))))

(defmethod initialize-instance :after
    ((lf list-frame) &key (dir :h) (box (make-box dir)) widgets)
  (declare (type symbol dir) (type list widgets))
  (setf (slot-value lf 'top-box) box)
  (gtk:container-add lf box)
  (dolist (w widgets)
    (gtk:box-pack-start box w))
  lf)

(defclass list-window (gtk:gtk-window)
  ((frame :type list-frame))
  (:metaclass gtk:gobject-class)
  (:documentation "list-frame, but then a window."))

(defmethod initialize-instance :after
    ((win list-window) &key (dir :h) (box (make-box dir)) widgets)
  (with-slots (frame) win
    (setf frame (make-instance 'list-frame :box box :widgets widgets))
    (gtk:container-add win frame)
    win))

(defclass list-expander (gtk:gtk-window)
  ((frame :type list-frame))
  (:metaclass gtk:gobject-class)
  (:documentation "list-frame, but then a window."))

(defmethod initialize-instance :after
    ((win list-expander) &key (dir :v) (box (make-box dir)) widgets)
  (with-slots (frame) win
    (setf frame (make-instance 'list-frame :box box :widgets widgets))
    (gtk:container-add win frame)
    win))

(defclass entry-with-setter (gtk:frame)
  ((box   :initform (make-instance 'gtk:h-box) :type gtk:h-box)
   (to-default-button :type gtk:button)
   (entry :type gtk:entry)
   (text-fn :type (function () string)))
  (:documentation
   "An entry with a button to set it according to a function.
 (At initiation, you can enter a straight with :text, :clear makes it\
 clear the screen.")
  (:metaclass gtk:gobject-class))
   
(defmethod initialize-instance :after
    ((entry-wd entry-with-setter)
     &key clear (set (if clear "" "Default"))
          (set-fn (lambda () set)) (text (funcall set-fn))
          (label (if clear "Clear" "Default")))
  (with-slots (box to-default-button entry) entry-wd
;    (setf box (make-instance 'gtk:h-box))
    (gtk:container-add entry-wd box) ;Attach frame and box.
   ;Make elements.
    (setf to-default-button (make-instance 'gtk:button :label label)
	  entry (make-instance 'gtk:entry :text text))
   ;Attach elements.
    (gtk:box-pack-start box to-default-button)
    (gtk:box-pack-start box entry)
   ;Make function of button.
    (gobject:g-signal-connect to-default-button "clicked"
      (lambda (b)
	(declare (ignore b))
	(setf (gtk:entry-text entry) (funcall set-fn)))))
  entry-wd)
  

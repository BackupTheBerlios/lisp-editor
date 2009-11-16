(in-package :cl)

(defpackage :gtk-stuff
  (:use :common-lisp :generic)
  (:export
   list-bin list-frame list-window list-expander
   entry-with-setter
  ;Note: use gtk:notebook instead of below.
   changing-widget changer-tab widget))

(in-package :gtk-stuff)

(defgeneric widget (obj)
  (:documentation "Getting widget within an object."))
(defgeneric (setf widget) (to obj)
  (:documentation "Settting a widget within an object,\
 possibly changing things such that it is shown."))

(defgeneric finalize-instance (instance))

(defmethod finalize-instance (instance) (values))

(defclass with-list ()
  ((box :type (or gtk:v-box gtk:h-box))
   (widgets :type list :initarg :widgets))
  (:documentation "Allows a frame/etc to have a list of widgets instead\
 of just one. Cannot be used by itself."))

(defun make-box (dir)
  "Keyword to h-box and v-box instances."
  (make-instance (case dir (:v 'gtk:v-box) (:h 'gtk:h-box) (t  dir))))

(defmethod initialize-instance :after
    ((wl with-list) &key (dir :h) (box (make-box dir)))
  (declare (type symbol dir) (type list widgets))
  (setf (slot-value wl 'box) box)
  (gtk:container-add wl box)
  (dolist (w (slot-value wl 'widgets))
    (gtk:box-pack-start box w))
  wl)

(defmethod finalize-instance ((wl with-list))
  (with-slots (box widgets) wl
    (gtk:container-remove wl box)
    (dolist (w (slot-value wl 'widgets))
      (gtk:container-remove box w)))
  (values))

(defclass list-bin (with-list gtk:bin)
  () (:metaclass gtk:gobject-class)
  (:documentation
   "A frame that can be directly filled with a list of stuff.."))

(defclass list-frame (with-list gtk:frame)
  () (:metaclass gtk:gobject-class)
  (:documentation
   "A frame that can be directly filled with a list of stuff.."))

(defclass list-window (with-list gtk:gtk-window)
  () (:metaclass gtk:gobject-class)
  (:documentation "list-frame, but then a window."))

(defclass list-expander (with-list gtk:gtk-window)
  () (:metaclass gtk:gobject-class)
  (:documentation "list-frame, but then a window."))

(defclass widget-at-demand (gtk:frame)
  ((widget-fn :type (function () t)))
  (:metaclass gtk:gobject-class)
  (:documentation "Makes the widget only when displayed."))

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

(defclass selector (gtk:frame)
  ((box   :initform (make-instance 'gtk:v-box) :type gtk:h-box)
   (entry :type gtk:entry)
   (cw    :type changing-widget)
   (choice-list :type list :initarg :choice-list)
   (choices :type list-frame))
  (:metaclass gtk:gobject-class)
  (:documentation ""))

(defmethod widget ((selector selector))
  (widget (slot-value selector 'cw)))
(defmethod (setf widget) (to (selector selector))
  (setf (widget (slot-value selector 'cw)) to))

(defun choice-hider (by-string choice-list
		     &key (choice-frame 'list-frame) (dir :v))
  (let*(set-fn
	(widgets
	 (mapcan (lambda (c)
		   (when (search by-string (car c))
		     (setf set-fn (widget-fn-choice c))
		     (list (cadr c))))
		 choice-list)))
    (cond
      ((null widgets)
       (make-instance 'gtk:label :label "None match."))
      ((null (cdr widgets)) ;Show matching one.
       (funcall set-fn))
      (t ;Show list of remaining matchers.
       (make-instance choice-frame :dir dir :widgets widgets)))))

(defun widget-fn-choice (c)
  (destructuring-bind (&key widget (widget-fn
				    (when widget
				      (lambda () widget)))) (cddr c)
    widget-fn))

(defmethod initialize-instance :after
    ((sel selector) &key (choice-frame 'list-frame) (by-string ""))
  (with-slots (box entry cw choices choice-list) sel
  ;Make some of the required widgets.
    (setf choices (make-instance choice-frame :dir :v
		    :widgets (mapcar #'cadr choice-list))
	  cw (make-instance 'changing-widget :widget choices)
	  entry (make-instance 'gtk:entry :text by-string))
  ;Put together.
    (gtk:container-add sel box)
    (gtk:box-pack-start box entry)
    (gtk:box-pack-start box cw)
  ;Make widgets send choices to change the cw.
    (dolist (c choice-list)
      (gobject:g-signal-connect (cadr c) (if-use (getf (cddr c) :signal)
						 "clicked")
	(lambda (&rest stuff)
	  (declare (ignore stuff)) ;Set itself as main.
	  (setf (widget cw) (funcall (widget-fn-choice c))))))
  ;Add response to signals to entry.
    (flet ((update (&rest stuff)
	     (declare (ignore stuff))
	     (finalize-instance choices)
	     (setf choices (choice-maker (gtk:entry-text entry) choice-list
					 :choice-frame choice-frame)
		   (widget cw) choices)
	     nil)
	   (activate (&rest stuff)
	     "Activating it picks the top choice."
	     (declare (ignore stuff))
	     (setf (widget cw)
		   (dolist (c choice-list)
		     (when (search (gtk:entry-text entry) (car c))
		       (return (funcall (widget-fn-choice c))))))
	     nil)
	   (entry-connect (fn &rest sigs)
	     (dolist (sig sigs)
	       (gobject:g-signal-connect entry sig fn))))
      (entry-connect #'update "button-press-event"
		     "key-press-event" "cut-clipboard" "paste-clipboard")
      (entry-connect #'activate "activate")))
  sel)
    

;;TODO make it work.
(defclass linear-number-maker (gtk:frame)
  ((box :type gtk:v-box :initform (make-instance 'gtk:v-box))
   (spin-button :type gtk:spin-button)
   (h-scale :type gtk:h-scale))
  (:metaclass gtk:gobject-class)
  (:documentation "A linked pair, spin-button and h-scale on a frame."))

(defmethod initialize-instance :after
    ((lnm linear-number-maker) &key adjustment)
  (with-slots (box spin-button h-scale) lnm
   ;Make spin-button and h-scale.
    (setf spin-button (make-instance 'gtk:spin-button
				     :adjustment adjustment)
	  h-scale (make-instance 'gtk:h-scale :adjustment adjustment))
   ;Put them together.
    (gtk:container-add lnm box)
    (gtk:box-pack-start box spin-button)
    (gtk:box-pack-start box h-scale)
   ;synchronize values.
    (gobject:g-signal-connect spin-button "change-value" 
      (lambda (&rest stuff)
	(declare (ignore stuff))
	(setf ;Transfer the value.
;	 (gtk:g-object-call-get-property (gtk:pointer h-scale) "value")
	 (gtk:range-value h-scale)
	 (gtk:spin-button-value spin-button))
	(gtk:widget-show h-scale)))
    (gobject:g-signal-connect h-scale "change-value"
      (lambda (&rest stuff)
	(declare (ignore stuff))
	(setf ;Transfer the value.
	 (gtk:spin-button-value spin-button)
	 (gtk:range-value h-scale))
;	 (gtk:g-object-call-get-property (gtk:pointer h-scale) "value"))
	(gtk:widget-show h-scale))))
  lnm)

(defmethod value ((lnm linear-number-maker))
  (gtk:spin-button-value (slot-value nmn 'spin-button)))
(defmethod (setf value) ((to number) (lnm linear-number-maker))
  (let ((to (coerce to 'double-float)))
    (with-slots (spin-button h-scale) lmn
      (setf (gtk:spin-button-value spin-button) to
	    (gtk:g-object-call-get-property (gtk:pointer h-scale) "value")
	    to))))

;;TODO automatic adding of adjustment doesn't seem to work.
(defclass with-adjustment ()
  ()
  (:documentation "Base class to provide decent default values and\
 ease the creation of gtk:adjustment element of the scales.
TODO base-class approach doesn't seem to work here.."))

(defmethod initialize-instance :after
    ((wa with-adjustment) &key adjustment
     (lower 0d0) (upper 1d0) (value 5d-1)
     (step-increment 1d-1) (page-increment 1d0) (page-size 1d0))
  (change-class wa (type-of wa)
    :adjustment (if-use adjustment
		  (make-instance 'gtk:adjustment
		    :lower lower :upper upper :value value
		    :page-increment page-increment :page-size page-size
		    :step-increment step-increment))))

(defmacro help-adjustment (name of &key doc)
  "Makes adjuster, as derivation from with-adjustment didn't seem to work.
TODO This doesn't work either :/"
  `(progn
     (defclass ,name (,of)
       () (:metaclass gtk:gobject-class)
       (:documentation ,doc))
     (defmethod initialize-instance :after
	 ((wa ,name) &key adjustment
	  (lower 0d0) (upper 1d0) (value 5d-1)
	  (step-increment 1d-3) (page-increment 1d0) (page-size 1d0))
       (change-class wa ',name
	 :adjustment (if-use adjustment
		       (make-instance 'gtk:adjustment
			 :lower lower :upper upper :value value
			 :page-increment page-increment :page-size page-size
			 :step-increment step-increment))))))

(help-adjustment v-scale gtk:v-scale
  :doc "V-scale with easier gtk:adjustment.(See with-adjustment)")
(help-adjustment h-scale gtk:h-scale
  :doc "H-scale with easier gtk:adjustment.(See with-adjustment)")
(help-adjustment spin-button gtk:spin-button
  :doc "Spin-button with easier gtk:adjustment.(See with-adjustment)")

;;NOTE use gtk:notebook!! (gtk:notebook-add-page adds a page.)
;; This one i only kept because notebooks seem to not allow vertical tabs.
(defclass changing-widget (gtk:frame)
  ((widget :initarg :widget :initform (make-instance 'gtk:frame)))
  (:documentation "A widget which you can easily swap with another widget.
gtk:notebook is probably better.")
  (:metaclass gtk:gobject-class))

(defmethod initialize-instance :after
    ((cw changing-widget) &key)
  (gtk:container-add cw (slot-value cw 'widget))
  cw)

(defmethod widget ((cw changing-widget))
  (slot-value cw 'widget))

(defmethod (setf widget) (to (cw changing-widget))
  (with-slots (widget) cw
    (gtk:container-remove cw widget)
    (gtk:container-add cw to)
    (gtk:widget-show widget)
    (setf widget to)))
    

(defclass changer-tab (gtk:toggle-button)
  ((widget-function)
   (changer :initarg :changer))
  (:documentation "A 'tab' that determines what box is in a changing box.\
 TODO untoggle those that should be untoggled.
gtk:notebook is probably better.")
  (:metaclass gtk:gobject-class))

(defmethod initialize-instance :after
    ((ct changer-tab) &key widget (widget-fn (lambda () widget)))
  (with-slots (changer widget-function) ct
    (setf widget-function widget-fn)
    (gobject:g-signal-connect ct "clicked"
      (lambda (b)
	(declare (ignore b))
	(setf (widget changer) (funcall widget-function)))))
  ct)

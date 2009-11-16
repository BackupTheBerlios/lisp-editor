(in-package :cl)

(defpackage :gtk-stuff
  (:use :common-lisp :generic)
;  (:nick-name :gtks)
;  (:import-from :gobject gobject:gobject-class)
  (:export
   widgets widget
   list-bin list-frame list-alignment list-window list-expander
   entry-with-setter
   selector frame-selector
   changer-tab))

(in-package :gtk-stuff)

(defgeneric (setf widgets) (to wl &key no-show)
  (:documentation "Change widgets."))
(defgeneric (setf widget) (to wl &key no-show)
  (:documentation "Single-widget version of setf widgets, replaces whole\
 list with just one. Some classes appropriate it."))

(defun pack-frame (frame widgets &optional (box (make-instance 'gtk:v-box)))
  (when frame (gtk:container-add frame box))
  (dolist (w widgets)
    (gtk:box-pack-start box w))
  (values))

(defclass with-list ()
  ((box :type (or gtk:v-box gtk:h-box))
   (widgets :type list :initform nil :reader widgets))
  (:documentation "Allows a frame/etc to have a list of widgets instead\
 of just one. Cannot be used by itself.
Also allows it to be set with setf widgets and setf widget"))

(defmethod (setf widgets) ((set-widgets list) (wl with-list)
			   &key no-show (expand t) (fill t))
  (with-slots (box widgets) wl
    (dolist (w widgets) ;Deconnect old.
      (gtk:container-remove box w))
    (dolist (w set-widgets) ;Connect old.
      (gtk:box-pack-start box w :expand expand :fill fill))
    (unless no-show
      (gtk:widget-show wl))
    (setf widgets set-widgets))) ;Replace.

(defgeneric widget (wl)
  (:documentation "Single-widget version of widgets, looks at first one."))
(defmethod widget ((wl with-list))
  (car (widgets wl)))
(defmethod (setf widget) (to (wl with-list) &key no-show)
  (setf (widgets wl :no-show no-show) (list to))
  to)

(defun make-box (dir)
  "Keyword to h-box and v-box instances."
  (make-instance (case dir (:v 'gtk:v-box) (:h 'gtk:h-box) (t  dir))))

(defmethod initialize-instance :after
    ((wl with-list)
     &key (dir :h) widget widgets (box (make-box dir)) no-top)
  (declare (type symbol dir) (type list widgets))
  (setf (slot-value wl 'box) box) ;Set box.
  (unless no-top
    (gtk:container-add wl box))
  (setf (slot-value wl 'widgets)
	(cond
	  ((and widgets widget)
	   (error "May only use either widget or widgets."))
	  (widgets widgets)
	  (widget  (list widget))))
  (dolist (w (slot-value wl 'widgets))
    (gtk:container-add box w))
  wl)

(defmethod finalize-instance ((wl with-list))
  (with-slots (box widgets) wl
    (gtk:container-remove wl box)
    (dolist (w (slot-value wl 'widgets))
      (gtk:container-remove box w)))
  (values))

(defclass list-bin (with-list gtk:bin)
  () (:metaclass gobject:gobject-class)
  (:documentation
   "A frame that can be directly filled with a list of stuff.."))

(defclass list-frame (with-list gtk:frame)
  () (:metaclass gobject:gobject-class)
  (:documentation
   "A frame that can be directly filled with a list of stuff.."))

(defclass list-alignment (with-list gtk:alignment)
  () (:metaclass gobject:gobject-class)
  (:documentation
   "A frame that can be directly filled with a list of stuff.."))

(defclass list-window (with-list gtk:gtk-window)
  () (:metaclass gobject:gobject-class)
  (:documentation "list-frame, but then a window."))

(defclass list-expander (with-list gtk:gtk-window)
  () (:metaclass gobject:gobject-class)
  (:documentation "list-frame, but then a window."))

;;-------------------------Entry with setter--------------------------------
(defclass entry-with-setter (list-alignment)
  ((to-default-button :type gtk:button)
   (entry :type gtk:entry))
  (:documentation
   "An entry with a button to set it according to a function.
 (At initiation, you can enter a straight with :text, :clear makes it\
 clear the screen.")
  (:metaclass gobject:gobject-class))
   
(defmethod initialize-instance :after
    ((entry-wd entry-with-setter)
     &key clear (set (if clear "" "Default"))
          (set-fn (lambda () set)) (text (funcall set-fn))
          (label (if clear "Clear" "Default")))
  (with-slots (box to-default-button entry) entry-wd
   ;Make elements.
    (setf to-default-button (make-instance 'gtk:button :label label)
	  entry (make-instance 'gtk:entry :text text))
    (setf (widgets entry-wd :no-show t)
	  (list to-default-button entry))
   ;Make function of button.
    (gobject:g-signal-connect to-default-button "clicked"
      (lambda (b)
	(declare (ignore b))
	(setf (gtk:entry-text entry) (funcall set-fn)))))
  entry-wd)

;;-------------------------Selectors----------------------------------------
(defclass selector (gtk:entry)
  ((cw    :type list-alignment :initarg :cw
	  :initform (make-instance 'list-alignment))
   (choice-list :type list :initarg :choice-list)
   (choices :type with-list))
  (:metaclass gobject:gobject-class)
  (:documentation ""))

(defmethod widget ((selector selector))
  (widget (slot-value selector 'cw)))
(defmethod (setf widget) (to (selector selector) &key no-show)
  (setf (widget (slot-value selector 'cw) :no-show no-show) to))

(defun choice-hider (by-string choice-list choices)
  (let (set-fn (cnt 0))
    (with-slots (widgets box) choices
      (dolist (c choice-list)
	(destructuring-bind (by-name chooser &key widget
			     (widget-fn
			      (when widget
				(lambda () widget)))) c
	  (gtk:container-remove box chooser)
	  (when (search by-string by-name)
	    (setf cnt (+ cnt 1)
		  set-fn widget-fn)
	    (gtk:box-pack-start box chooser :expand nil)))))
    (case cnt
      (0 (make-instance 'gtk:label :label "None match."))
      (1 (funcall set-fn))
      (t  (slot-value choices 'box)))))

(defun widget-fn-choice (c)
  (destructuring-bind (&key widget (widget-fn
				    (when widget
				      (lambda () widget)))) (cddr c)
    widget-fn))

(defmethod initialize-instance :after
    ((sel selector) &key (choice-frame 'with-list) no-signal)
  (with-slots (cw choices choice-list) sel
  ;Make some of the required widgets.
    (setf choices (make-instance choice-frame :dir :v :no-top t))
    (setf (widget cw)
	  (choice-hider (gtk:entry-text sel) choice-list choices))
  ;Make widgets send choices to change the cw.
    (unless no-signal
      (dolist (c choice-list)
	(gobject:g-signal-connect (cadr c) (if-use (getf (cddr c) :signal)
						   "clicked")
	  (lambda (&rest stuff)
	    (declare (ignore stuff)) ;Set itself as main.
	    (setf (widget cw) (funcall (widget-fn-choice c)))))))
  ;Add response to signals to entry.
    (flet ((update (&optional e event &rest stuff)
	     (declare (ignore e stuff))
	     (let ((text       ;Couldn't figure out how to do it after, so
		    (if event  ;fishing out the last character.
		      (with-slots (gtk:type gtk:string) event
			(case gtk:type
			  (:key-press (concatenate 'string
						   (gtk:entry-text sel)
						   gtk:string))
			  (t (gtk:entry-text sel))))
		      (gtk:entry-text sel))))
	       (setf (widget cw)
		     (choice-hider text choice-list choices)))
	     nil)
	   (activate (&rest stuff)
	     "Activating it picks the top choice."
	     (declare (ignore stuff))
	     (setf (widget cw)
		   (dolist (c choice-list)
		     (when (search (gtk:entry-text sel) (car c))
		       (return (funcall (widget-fn-choice c))))))
	     nil)
	   (entry-connect (fn &rest sigs)
	     (dolist (sig sigs)
	       (gobject:g-signal-connect sel sig fn))))
      (entry-connect #'update "button-press-event"
		     "key-press-event" "cut-clipboard" "paste-clipboard")
      (entry-connect #'activate "activate")))
  sel)

(defclass frame-selector (list-alignment)
  ((sel :type selector))
  (:default-initargs :dir :v :no-top nil)
  (:metaclass gobject:gobject-class)
  (:documentation "Selector with a frame to show option.
Appropriates widget to mean the curret widget active."))

(defmethod widget ((sel frame-selector))
  (widget (slot-value sel 'sel)))
(defmethod (setf widget) (to (sel frame-selector) &key no-show)
  (setf (widget (slot-value sel 'sel) :no-show no-show) to))

(defmethod initialize-instance :after
    ((selector frame-selector)
     &key choice-list (cw (make-instance 'list-alignment)) no-signal
         (init (make-instance 'selector :choice-list choice-list :cw cw
			      :no-signal no-signal))
         out-of-frame)
  (with-slots (sel) selector
    (setf sel init)
    (setf (widgets selector) ;Note the s!
	  (list sel (slot-value sel 'cw))))
  selector)

;;--------------------------------------------------------------------------

;;TODO make it work.
(defclass linear-number-maker (gtk:frame)
  ((box :type gtk:v-box :initform (make-instance 'gtk:v-box))
   (spin-button :type gtk:spin-button)
   (h-scale :type gtk:h-scale))
  (:metaclass gobject:gobject-class)
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
       () (:metaclass gobject:gobject-class)
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

;;NOTE You can quite easily make notebook functionality with these, 
;; probably notebook itself better, though!

;;TODO improve to allow for multiple?
(defclass changer-tab (gtk:toggle-button)
  ((widget-function)
   (changer :initarg :changer))
  (:documentation "A 'tab' that determines what box is in a changing box.\
 TODO untoggle those that should be untoggled.
gtk:notebook is probably better.")
  (:metaclass gobject:gobject-class))

(defmethod initialize-instance :after
    ((ct changer-tab) &key widget (widget-fn (lambda () widget)))
  (with-slots (changer widget-function) ct
    (setf widget-function widget-fn)
    (gobject:g-signal-connect ct "clicked"
      (lambda (b)
	(declare (ignore b))
	(setf (widget changer) (funcall widget-function)))))
  ct)

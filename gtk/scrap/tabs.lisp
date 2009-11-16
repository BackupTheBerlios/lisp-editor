(defpackage :gtk-stuff
  (:use :common-lisp :generic)
  (:export frame-box tabs-like defaulted-entry))

(in-package :gtk-stuff)

(defclass list-frame (gtk:frame)
  ((top-box :accessor box))
  (:documentation
   "A frame that can be directly filled with a list of stuff..")
  (:metaclass gtk:gobject-class))

(defmethod initialize-instance
    ((lf list-frame) &key (box-dir :h) (box (make-instance
					       (case box-dir
						 (:v 'gtk:v-box)
						 (:h 'gtk:h-box)
						 (t  box-dir))))
     widgets)
  (declare (type symbol box-dir) (type list widgets))
  (with-slots (top-box) lf
    (setf top-box box)
    (gtk:container-add lf box)
    (dolist (w widgets)
      (gtk:box-pack-start box w))
    lf))

(defclass changing-box (list-frame)
  ((cur-box)); :type (or gtk:v-box gtk:h-box)))
  (:documentation "A box which you can easily swap with another box.")
  (:metaclass gtk:gobject-class))

(defmethod box ((cb changing-box))
  (slot-value cb 'cur-box))

(defmethod (setf box) ((to gtk:h-box) (cb changing-box))
  (with-slots (cur-box) cb
    (gtk:container-remove cb cur-box)
    (gtk:container-add cb to)
    (setf cur-box to)))

(defclass tab-changer (gtk:toggle-button)
  ((the-box) (changable))
  (:documentation "A 'tab' that determines what box is in a changing box.")
  (:metaclass gtk:gobject-class))

(defmethod initialize-instance
    ((tc tab-changer) &key (box-dir :h) (box (make-instance
					       (case box-dir
						 (:v 'gtk:v-box)
						 (:h 'gtk:h-box)
						 (t  box-dir))))
     (changing-box (error "tabs-changer requires a to be able to switch\
 boxes of something."))
     (g-signal "clicked"))
  (with-slots (the-box changable) tc
    (setf the-box box
	  changable changing-box)
    (gobject:g-signal-connect tc g-signal
      (lambda (b)
	(declare (ignore b))
	(setf (box changable) the-box)))
    tc))

(defun tabbed ()
  (let*((ch-box (make-instance 'changing-box))
	(tab-a (make-instance 'tab-changer :changing-box ch-box))
	(tab-b (make-instance 'tab-changer :changing-box ch-box)))
    (gtk:widget-show (make-instance 'list-frame
		       :widgets 

#|
 (defclass tabbed-frame (frame)
  ((top-box) (tab-frame)
   (tab-dependend :type changing-box)))

 (defmethod initialize-instance
    ((tf tabbed-frame) &key (tab-dir :h) above-changing below-changing
     (box (make-instance
	   (case box-dir (:v 'gtk:v-box) (:h 'gtk:h-box) (t  box-dir)))))
  (with-slots (top-box tab-frame) cb
    |#

(defun frame-box (&key (dir 'gtk:h-box) box 
		  (frame (make-instance 'gtk:frame)))
  (let ((box (if-use box
		     (make-instance (case dir ((:h :hr) 'gtk:h-box)
					      ((:v :vd) 'gtk:v-box)
					      (t  dir))))))
    (gtk:container-add frame box)
    (values frame box)))


(defun tabs-like
    (tabs &key (tabs-dir 'gtk:h-box) tabs-box
     (v-box (make-instance (case tabs-dir
			     ((:h :hr gtk:h-box) 'gtk:v-box)
			     ((:v :vd gtk:h-box) 'gtk:h-box)
			     (t 'gtk:v-box))))
     (initial-tab (car tabs))
     pre-tabs
     (parent (make-instance 'gtk:gtk-window
	       :type :toplevel :window-position :center
	       :title "Tabbed window"
	       :default-width 300 :default-height 100)))
  "Tabs are (list name command) the command much return something that use\
 be gtk:box-pack-start
Returns the parent of the whole thing."
  (let ((active-tab initial-tab)
	 (frame nil))
   (multiple-value-bind (tabs-frame tabs-box)
       (frame-box :dir tabs-dir :box tabs-box)
     (gtk:container-add parent v-box)
     (gtk:box-pack-start v-box tabs-frame) ;Set up tabs.
     
     (dolist (w pre-tabs)
       (gtk:box-pack-start tabs-box pre-tabs))
     
     (dolist (tab tabs) ;Handle tabs
       (gtk:box-pack-start tabs-box (car tab)) ;Add tabs
       (gobject:g-signal-connect (car tab) (if-use (caddr tab) "clicked")
	(lambda (b)
	  (declare (ignore b))
	  
	  (unless (equal (car active-tab) (car tab))
	    (setf (gtk:toggle-button-active (car active-tab)) nil)
	    (setq active-tab tab))
	  
	  (when frame ;Remove existing.
	    (gtk:container-remove v-box frame))
	  (setq frame (funcall (cadr tab))) ;Set new.
	  (gtk:box-pack-start v-box frame) ;pack new.
	  (gtk:widget-show parent :all t))))
   ;Enable initial tab, should make frame.
     (setf (gtk:toggle-button-active (car active-tab)) t)))
  
  parent)

(defun defaulted-entry (default &key default-text
			(entry (make-instance 'gtk:entry)))
  (if default-text
    (multiple-value-bind (frame box) (frame-box :dir :h)
      (gtk:box-pack-start box frame)
      (let ((default-button (make-instance 'gtk:button :label "Default")))
	(gobject:g-signal-connect default-button "clicked" 
	  (lambda (b)
	    (declare (ignore b))
	    (setf (gtk:entry-text entry) default-text)))
	(gtk:box-pack-start box default-button)
	(gtk:box-pack-start box entry))
      frame)
    entry))


(in-package :gtk-stuff)

;;list-frame and list-window.
(gtk:within-main-loop
  (gtk:widget-show (make-instance 'list-window :box-dir :v
		     :widgets (mapcar (lambda (n)
					(make-instance 'gtk:button :label n))
				      '("A" "Bee" "Cee" "Deee")))))
;Changing-box.
(gtk:within-main-loop
  (let*((changing-widget
	 (make-instance 'list-alignment
	    :widget (make-instance 'gtk:button :label "MIAUW"))))
  (macrolet ((mk-tab (name widget &rest args)
	       `(make-instance 'changer-tab
		  :changer changing-widget :label ,name :widget ,widget
		  ,@args)))
  (let*((v-scale
	 (make-instance 'gtk:v-scale :digits 3; :upper 1d1
	   :adjustment (make-instance 'gtk:adjustment
			  :lower 0d0 :upper 1d1)))
	(jump
	 (make-instance 'gtk:button :label "J"))
	(model (make-instance 'gtk:array-list-store))
	(cbox
	 (make-instance 'gtk:combo-box-entry
	   :model (make-instance 'gtk:tree-view :model model)))
	(tabs
	 `(,(mk-tab "entry" (make-instance 'entry-with-setter
					   :set "init" :text "lala"))
	   ,(mk-tab "Scaler" (make-instance 'gtk:scale-button))
	   ,(mk-tab "jump-0.15" jump)
	   ,(mk-tab "V-scale" v-scale)
	   ,(mk-tab "number" (make-instance 'linear-number-maker))
;	   ,(mk-tab "H-scale" ;TODO memory error? figure out if also next session.
;		    (make-instance 'h-scale :upper 1d0))
;	   ,(mk-tab "Spin-button" ;TODO doesn't spin.
;		    (make-instance 'spin-button :digits 4))
	   ,(mk-tab "Font sel"
		    (make-instance 'gtk:font-selection))
	   ,(mk-tab "cbox" cbox)
	   ,(mk-tab "exp"
	      (let ((entry (make-instance 'gtk:entry))
		    (expander
		     (make-instance 'list-expander :label "expander"
		       :widgets (list (make-instance 'gtk:label
						     :label "label")
				      (make-instance 'gtk:button
						     :label "blah")))))
		(gobject:g-signal-connect entry "button-press-event"
		  (lambda (&rest whatever)
		    (declare (ignore whatever))
		    (print 'entry)
		    (setf- not (gtk:expander-expanded expander))))
		(make-instance 'list-frame :dir :v
 		  :widgets (list entry expander))))
	   ,@(mapcar
	      (lambda (n)
		(mk-tab n (make-instance 'gtk:button :label n)))
	      '("A" "1" "2" "3"))))
	(win (make-instance 'list-window :dir :v :title "tabbed"
	       :widgets (list (make-instance 'gtk:label :label "miauw")
			      (make-instance 'list-frame
				:widgets (list (make-instance 'list-frame
						 :dir :v :widgets tabs)
					       changing-widget))))))
    (gobject:g-signal-connect jump "click"
      (lambda (&rest stuff)
	(gtk:emit-signal signal "change-value" :jump 15d-1)
	nil))
#|  (gobject:g-signal-connect v-scale "change-value"
      (lambda (&rest stuff)
	(push stuff *meh*)
	nil))|#
    (gtk:scale-add-mark v-scale 4d0 :left "4d0")
#|    (gobject:g-signal-connect (slot-value v-scale 'widget) "format-value"
	(lambda (b)
	  (declare (ignore b))
;	  (print b)
	  (gtk:widget-show v-scale)))) |#
    (gtk:widget-show win)))))

;Trying out notebook.
(gtk:within-main-loop
  (let ((notebook (make-instance 'gtk:notebook))
	(box (make-instance 'gtk:v-box)))
    (gtk:notebook-add-page ;NOTE: should it mirror the gkt docs?
     notebook (make-instance 'gtk:button :label "yay1")
     (make-instance 'gtk:label :label "yay1"))
    (gtk:notebook-add-page
     notebook (make-instance 'gtk:button :label "yay2")
     (make-instance 'gtk:label :label "yay2"))
    (gtk:box-pack-start box (make-instance 'gtk:button :label "miauw"))
    (gtk:box-pack-start box (make-instance 'gtk:button :label "miauw"))
    (gtk:notebook-add-page
     notebook box (make-instance 'gtk:label :label "Cat"))
    (gtk:widget-show (make-instance 'list-window :title "Notebook"
		        :widgets (list notebook)))))

;Playing with some events.
(gtk:within-main-loop
  (let ((button (make-instance 'gtk:toggle-button :label "miauw"))
	(entry (make-instance 'gtk:entry)))
    (flet ((s (str)
	     (gobject:g-signal-connect entry str
	       (lambda (&rest stuff) (push (cons str stuff) *meh*)
		       nil))))
      (s "insert-at-cursor") (s "button-press-event")
      (s "cut-clipboard") (s "delete-from-cursor") (s "backspace")
      (s "paste-clipboard")
      (s "activate") (s "key-press-event"))
    (gtk:widget-show (make-instance 'list-window :dir :v
		      :widgets (list button entry)))))

(progn (print *meh*)
       (setf *meh* nil))

(progn (gtk:within-main-loop
	 (gtk:widget-show (make-instance 'list-window :dir :v
 			    :widgets (car *meh*))))
       (print (mapcar (gtk:name (car meh)

(defvar *meh* nil)
(defun meh (x) (push x *meh*) x)

;;Playing with markup.
(gtk:within-main-loop
  (gtk:widget-show
   (let ((lbl (make-instance 'gtk:label
	         :label "<b><i>LALA</i></b>" ;:xalign 0.0
		 :use-markup t)))
     (make-instance 'list-window :dir :v :widgets (list lbl)))))

;;Selector.
(defun basic-button-signal (name)
  (list name (make-instance 'gtk:button :label name :relief :none
			    :xalign 0.0)
	:widget (make-instance 'gtk:button :label name)))

(gtk:within-main-loop
  (gtk:widget-show
   (make-instance 'list-window :dir :v :title "Selector"
     :widget (make-instance 'frame-selector
	       :choice-list
	       (mapcar #'basic-button-signal
		       (list "A" "B" "miauw" "Cookie"
			     "Lookie" "Boekie" "Brains"))))))


(gtk:within-main-loop ;Hmm seem unable to draw..
  (let*((gc (make-instance 'gdk:graphics-context))
	(d  (make-instance 'gtk:drawing-area))
	(col (make-instance 'gdk:color)))
;    (setf (gdk:color-red col) 10)
;    (gdk:gdk-gc-set-rgb-fg-color gc col)
    (gobject:g-signal-connect d "expose_event"
      (lambda (&rest stuff)
	(declare (ignored stuff))
	(gdk:gdk-draw-line (gtk:widget-window d) gc
			   0 0 100 100)
	nil))
    (gtk:widget-show (make-instance 'list-window :width 100 :height 100
			:title "miauw" :widgets (list d)))))

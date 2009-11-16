(in-package :cl)

(defpackage :gtk-tiler
  (:use :common-lisp :generic :denest
	:gtk-stuff)
  (:export tiler
	   tiles-total-area tiles-align
	   button-tile)
  (:documentation
   "Visualization of size of things by subdividing a rectangle."))

(in-package :gtk-tiler)

(defclass tile ()
  ((name :initarg :name :initform "" :type string)
   (area :initarg :area :initform 1 :type number
     :documentation "Area of plus sub-tiles." :accessor area)
   (fn :initarg :fn :initform 0 :type function
     :documentation "Function that makes the widget accompanying the tile.")
   (child-tiles :initarg :subtiles :initform nil :type list
     :documentation "Tiles subordinate.")))	     

(defclass tiler (gtk:alignment)
  ((box :type (or gtk:v-box gtk:h-box)))
  (:metaclass gtk:gobject-class)
  (:documentation
   "Visualization of size of things by subdividing a rectangle."))

(defun tiles-total-area (tiles &key (area 0))
  "Calculates total area of tiles."
  (dolist (tile tiles area)
    (setf- + area (cadar tile))))

(defun tiles-align
    (tiles &key (total-area (tiles-total-area tiles))
                (small-ratio 0.1) (small-area (* small-ratio total-area))
                (recursive 10))
  "Realigns tiles such that those taking least space are the other way\
 around."
  (declare (type number total-area small-ratio small-area)
	   (type integer recursive))
  (when (< recursive 0)
    (return-from tiles-align tiles))
  (let*((tiles (sort tiles (lambda (a b) ;Sort them by size
			     (< (cadar a) (cadar b)))))
	(sum 0)
	(pos (position-if (lambda (tile)
			    (not(unless (> (cadar tile) small-area)
				  (setf- + sum (cadar tile)))))
			  tiles)))
    (if (= 0 pos)
      tiles
      (let ((small (subseq tiles 0 pos))
	    (large (subseq tiles pos)))
	(cons
	 (cons (list "small" sum)
	       (tiles-align small :total-area sum
		 :small-ratio small-ratio :recursive (- recursive 1)))
	 (tiles-align large :total-area (- total-area sum)
	   :small-ratio small-ratio :recursive (- recursive 1)))))))

(defun button-tile
    (&key (min-name-size 30) (relief :normal) (xalign 5d-1) (yalign 5d-1)
          signals)
  "Makes a function to produce a button tile."
  (lambda (name area w h subtiles)
    (let ((button
	   (make-instance 'gtk:button
	     :label (if (and (> w min-name-size) (> h min-name-size))
			name "") 
	     :relief relief :xalign xalign :yalign yalign
	     :width-request w :height-request h)))
      (dolist (s signals)
	(gobject:g-signal-connect button (car s) 
	  (lambda (&rest stuff)
	    (declare (ignore stuff))
	    (funcall (cadr s) name area w h subtiles))))
      button)))
     
(defmethod initialize-instance :after
    ((tiler tiler) &key tiles (total-area (tiles-total-area tiles))
     w h (dir :h) (dir-b (eql dir :v))
     (depth 3) (small-ratio 0.02) (small-area (* small-ratio total-area))
     (min-name-size 30)
     button-tile-signals
     (make-tile (button-tile :min-name-size min-name-size
			     :signals button-tile-signals)))
  (with-slots (box) tiler
    (setf box (make-instance (if dir-b 'gtk:v-box 'gtk:h-box)))
    (gtk:container-add tiler box)
    (dolist (tile tiles)
      (cond
	((listp tile)
	 (destructuring-bind ((name area &optional (mk-tile make-tile))
			      &rest subtiles) tile
	   (multiple-value-bind (nw nh)
	       (if dir-b (values w (floor (* h (/ area total-area))))
		         (values (floor (* w (/ area total-area))) h))
	     (gtk:box-pack-start box
	       (if (and (> depth 0) (< small-area area) subtiles)
		 (make-instance 'tiler :dir-b (not dir-b)
		   :total-area area
		   :w nw :h nh :depth (- depth 1) :small-area small-area
		   :make-tile make-tile
		   :tiles subtiles)
		 (funcall mk-tile name area nw nh subtiles)))))))))
  tiler)

(defun 

(when (find-package :cl-fad) ;TODO filesystem browser.
  )
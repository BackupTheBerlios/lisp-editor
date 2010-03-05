;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(in-package :cl)

(defpackage :gtk-tiler
  (:use :common-lisp :generic :denest
	:gtk-stuff)
  (:export tile
	   tiler pathed-tiler button-pathed-tiler
	   tiles-total-area
	   tiles-align tiles-ratio-align tiles-dud-hook
	   button-tile
	   child-hook-wrap-frame
	   make-directory-tiles)
  (:documentation
   "TODO fails to draw somehow.. Sometimes mysteriously works again..
TODO ratio-based subdividing not entirely right.
Visualization of size of things by subdividing a rectangle."))

(in-package :gtk-tiler)

(defclass tile ()
  ((name :initarg :name :initform "" :type string)
   (area :initarg :area :initform 1 :type number
     :documentation "Area of plus sub-tiles." :accessor area)
   (fn :initarg :fn :initform nil :type (or function nil)
     :documentation "Function that makes the widget accompanying the tile.")
   (child-tiles :initarg :subtiles :initarg :child-tiles
     :initform nil :type list
     :documentation "Tiles subordinate.")))

(defun follow-path (tiles path)
  "Follow a path through child tiles."
  (if (or (null tiles) (null path)) tiles
    (follow-path
     (dolist (tile tiles)
       (when (string= (car path) (slot-value tile 'name))
	 (return (slot-value tile 'child-tiles))))
     (cdr path))))

(defun tiles-equal (ta tb)
  (mapcar (lambda (tla tlb)
	    (unless (string= (slot-value tla 'name)
			     (slot-value tlb 'name))
	      (return-from tiles-equal)))
	  ta tb)
  (mapcar (lambda (tla tlb)
	    (unless (tiles-equal (slot-value tla 'child-tiles)
				 (slot-value tlb 'child-tiles))
	      (return-from tiles-equal)))
	  ta tb)
  (= (length ta) (length tb)))

(defun deduce-path (from-tiles to-tiles &key path)
  "Deduce the path, if any."
  (cond
    ((tiles-equal from-tiles to-tiles)
     path)
    ((not to-tiles)
     :failed)
    (t
     (dolist (tile from-tiles :failed)
       (with-slots (name child-tiles) tile
	 (let ((path (deduce-path child-tiles to-tiles
				  :path (cons name path))))
	   (unless (or (eql path :failed) (not path))
	     (return-from deduce-path path))))))))
	  

(defclass tiler (list-alignment)
  ((tile-widget-fn :type function)
   (init-tiles :initarg :tiles :type list)
   (tiles :type list :reader tiles))
  (:default-initargs :no-top nil)
  (:metaclass gobject:gobject-class)
  (:documentation
   "Visualization of size of things by subdividing a rectangle."))

(defun tiles-total-area (tiles &key (area 0))
  "Calculates total area of tiles."
  (dolist (tile tiles area)
    (setf- + area (area tile))))

(defgeneric (setf tiles) (set-to tiler &key)
  (:documentation "Set the tiles."))

(defmethod (setf tiles) ((set-to list) (tiler tiler) &key)
  (with-slots (tiles tile-widget-fn) tiler
    (setf (widgets tiler) (funcall tile-widget-fn set-to
				   :area (tiles-total-area set-to)))
    (setf tiles set-to)))

(defun tiles-align
    (tiles &key (total-area (tiles-total-area tiles))
                (small-ratio 0.1) (small-area (* small-ratio total-area))
                dir w h path)
  "Realigns tiles such that those taking least space are the other way\
 around.
Use generic:curry to conveniently enter as tiles-hook."
  (declare (type number total-area small-ratio small-area)
	   (ignore dir))
  (let*((tiles (sort tiles (lambda (a b) ;Sort them by size
			     (< (area a) (area b)))))
	(sum 0)
	(pos (if-use (position-if (lambda (tile)
				    (not(unless (> (area tile) small-area)
					  (setf- + sum (area tile)))))
				  tiles)
		     0)))
    (if (= 0 pos) tiles
	          (cons (cons sum (subseq tiles 0 pos))
			(subseq tiles pos)))))

(defun tiles-ratio-align (tiles &key dir w h path
			  (total-area (tiles-total-area tiles))
			  (max-ratio 1.5) (min-ratio 0.5))
  "Tile hook that realigns with the purpose."
  (declare (type number max-ratio min-ratio)
	   (type integer w h)
	   (type (or (eql :v) (eql :h)) dir))
  (denest
   (if (null tiles) tiles)
   (let ((whole-ratio (case dir (:v (/ h w)) (:h (/ w h))))
	 (leave-area 0)))
   (collecting (nil stay stay))
   (collecting (nil leave leave)
     (dolist (tile tiles)
       (cond
	 ((= total-area 0)
	  (return-from tiles-ratio-align tiles))
	 ((< min-ratio 
	     (* (/ (area tile) total-area) whole-ratio)
	     max-ratio)
	  (stay tile))
	 (t
	  (setf- + leave-area (area tile))
	  (leave tile))))
     (return-from tiles-ratio-align
       (flet ((split (list &optional (at (floor (/ (length list) 2))))
		(let ((first (subseq list 0 at))
		      (second (subseq list at)))
		  (if (null first) second
		      (list (cons (tiles-total-area first) first)
			    (cons (tiles-total-area second) second))))))
	 (cond
	   ((null stay)  (split leave))
	   ((null leave) (split stay))
	   (t  	         (cons (cons leave-area leave) stay))))))))

;TODO tile-aligner that takes into account the size of the label.

(defun tiles-dud-hook (tiles &key dir w h path)
  "Tiles-hook that does nothing."
  tiles)

(defgeneric button-tile (tiler &key))

(defmethod button-tile
    ((tiler tiler) &key (min-name-size 30) 
     (relief :normal) (xalign 5d-1) (yalign 5d-1)
     signals top-changer)
  "Makes a function to produce a button tile."
  (lambda (tile w h path)
    (with-slots (name) tile
      (let ((button
	     (make-instance 'gtk:button
	       :label (if (and (> w min-name-size) (> h min-name-size))
			  name "")
	       :relief relief :xalign xalign :yalign yalign
	       :width-request w :height-request h)))
	(dolist (s (if (not top-changer) signals
		       (cons (list "clicked"
				   (lambda (tile path)
				     (setf (tiles tiler)
					   (slot-value tile 'child-tiles))
				     nil))
			     signals)))
	  (gobject:g-signal-connect button (car s) 
	    (lambda (&rest stuff)
	      (declare (ignore stuff))
	      (funcall (cadr s) tile path))))
	button))))

(defun child-hook-wrap-frame (&optional how)
  "Child-hook that wraps a frame around."
  (lambda (tiler tile path)
    (declare (ignore path))
    (make-instance 'list-frame :widget tiler
		   :label (case how
			    (:named (slot-value tile 'name))
			    (t "")))))

;TODO long function... Don't see better alternative atm.
(defmethod initialize-instance :after
    ((tiler tiler) &key (base-tiler tiler)
     (tile-hook #'tiles-ratio-align)
 ;Width and height target, initial direction, and maximum depth.
     w h (depth 30) (dir :h) ;TODO align direction with frame dir!
 ;Total area
     (total-area (tiles-total-area (slot-value tiler 'init-tiles)))
 ;Small area is the area smaller then which no childs are to be displayed.
 ;Small ratio is used to calculate small-area with total-area
     (small-ratio 0.01) (small-area (* small-ratio total-area))
 ;Too-small ratio is for non-childs that are too small, and are to me
 ;put together under 'small' button.
     (too-small-ratio 0.01) 
     (too-small-area (* too-small-ratio total-area))
 ;Make-tile produces the tile, min-name-size and button-tile-signals
 ;entered in button-tile, as default.
     (min-name-size 30) button-tile-signals
     (make-tile (button-tile base-tiler
			     :min-name-size min-name-size
			     :signals button-tile-signals))
 ;Top tile, defaultly same as make-tile.
     (make-top-tile
      (button-tile base-tiler 
		   :min-name-size min-name-size
		   :signals button-tile-signals :top-changer t))
     (child-hook (lambda (tiler tile path) tiler))
 ;Path
     path)
  (flet 
      ((tiles-widgets (from-tiles &key area)
	 (denest
	  (multiple-value-bind (total-area small-area too-small-area)
	      (if area (values area
			      (* small-ratio area)
			      (* too-small-ratio area))
		       (values total-area small-area too-small-area)))
	  (labels
	      ((w-h (area)
		 (case dir
		   (:v (values w (floor (* h (/ area total-area)))))
		   (:h (values (floor (* w (/ area total-area))) h))))
	       (mk-tiler (area nw nh subtiles &optional push-path)
		 "Need to pass all the damn arguments on."
		 (make-instance 'tiler :tiles subtiles :base-tiler base-tiler
 		    :w nw :h nh :depth (- depth 1)
		    :dir (case dir (:v :h) (:h :v))
		    :total-area area
		    :small-area small-area :too-small-area too-small-area
		    :make-tile make-tile :make-top-tile make-top-tile
		    :path (cons push-path path)
		    :tile-hook tile-hook :child-hook child-hook))))
	  (mapcan
	   (lambda (tile)
	    (typecase tile
	      (cons ;TODO allow it to set direction too?
	       (denest
		(destructuring-bind (area &rest subtiles) tile)
		(multiple-value-bind (nw nh) (w-h area))
		(unless (or (= nw 0) (= nh 0)))
		(list)
		(cond
		  ((null subtiles)
		   (error "Subdivision without subtiles?!"))
		  ((null (cdr subtiles))
		   (with-slots (fn child-tiles) (car subtiles)
		     (funcall (if (null child-tiles)
				  (if-use fn make-tile) make-top-tile)
			      (car subtiles) nw nh path)))
		  ((< area too-small-area)
		   (funcall make-top-tile
			    (make-instance 'tile
			      :name (format nil "small~%(~D)"
					    (length subtiles))
			      :area area :child-tiles subtiles)
			    nw nh path))
		  (t
		   (mk-tiler area nw nh subtiles)))))
	      (tile
	       (denest
		(with-slots (area fn child-tiles) tile)
		(multiple-value-bind (nw nh) (w-h area))
		(unless (or (= nw 0) (= nh 0)))
		(list)
		(cond
		  ((null child-tiles)
		   (funcall (if-use fn make-tile)
			    tile nw nh path))
		  ((< area too-small-area)
		   (funcall make-top-tile
			    tile nw nh path))
		  ((or (< area small-area) (< depth 0))
		   (funcall make-top-tile
			    tile nw nh path))
		  (t
		   (funcall child-hook
			    (mk-tiler area nw nh child-tiles)
			    tile path)))))))
	  (funcall tile-hook from-tiles
		   :dir dir :w w :h h :total-area total-area :path path)))))
    (with-slots (init-tiles tiles tile-widget-fn) tiler
      (setf tiles init-tiles
	    tile-widget-fn #'tiles-widgets
	    (widgets tiler :no-show t) (tiles-widgets init-tiles))))
  tiler)

(defclass pathed-tiler (tiler)
  ((path :initform nil :type list :reader path))
  (:default-initargs :dir :v :no-top nil)
  (:metaclass gobject:gobject-class)
  (:documentation "Tiler that keeps track of the current path."))

(defmethod (setf tiles)
    ((set-to list) (pt pathed-tiler)
     &key (set-path (when-let
			ded (deduce-path (slot-value pt 'init-tiles) set-to)
		      (unless (symbolp ded) ded))))
  (with-slots (tiles tile-widget-fn
	       tab-frame path has-indicator) pt
    (setf path set-path
	  (widgets pt) (funcall tile-widget-fn set-to
				:area (tiles-total-area set-to))
	  tiles set-to)))

(defmethod (setf path) ((to-path list) (pt pathed-tiler))
  (with-slots (path init-tiles) pt
    (if-let to-tiles (follow-path init-tiles to-path)
      (setf (tiles pt :set-path to-path) to-tiles
	    path to-path)
      path)))

(defclass button-pathed-tiler (pathed-tiler)
  ((path-list :initform (make-instance 'list-frame :dir :h)
	      :type list-frame)
   (current-indicator :initform (make-instance 'gtk:label) :type gtk:label))
  (:default-initargs :no-top nil)
  (:metaclass gobject:gobject-class)
  (:documentation "Tiler that keeps track of the current path."))

;Make widget changer add the path-list.
(defmethod (setf widgets) :around
    ((to list) (bpt button-pathed-tiler) &key no-show)
  (call-next-method
   (append
    (with-slots (path-list current-indicator) bpt
      (list path-list current-indicator))
    to) bpt :no-show no-show))

(defmethod (setf tiles) :around
    ((set-to list) (bpt button-pathed-tiler)
     &key (set-path
	   (when-let ded (deduce-path (slot-value bpt 'init-tiles) set-to)
	     (unless (symbolp ded) ded))))
  (do ((p set-path (cdr p))
       (out nil
	    (cons (let ((button (make-instance 'gtk:button :label (car p)))
			(path (reverse(copy-list p))))
		    (gobject:g-signal-connect button "clicked"
		      (lambda (&rest stuff)
			(declare (ignore stuff))
			(setf (path bpt) path)
			nil))
		    button)
		  out)))
      ((null p)
       (when out
	 (setf (widgets (slot-value bpt 'path-list) :expand nil :fill nil)
	       (let ((button (make-instance 'gtk:button :label "Root")))
		 (gobject:g-signal-connect button "clicked"
		   (lambda (&rest stuff)
		     (setf (tiles bpt :set-path nil)
			   (slot-value bpt 'init-tiles))))
		 (cons button out))))))
  (call-next-method set-to bpt :set-path set-path))

(defmethod button-tile :around
    ((bpt button-pathed-tiler)
     &key (min-name-size 30) 
     (relief :normal) (xalign 5d-1) (yalign 5d-1)
     signals top-changer)
  (flet ((evt (tile path)
	   (declare (ignore path))
	   (setf (gtk:label-label (slot-value bpt 'current-indicator))
		 (with-slots (name) tile
		   (if-let i (position #\Newline name)
		     (subseq name 0 i) name)))
	   nil))
    (call-next-method bpt
      :signals (cons (list "enter-notify-event" #'evt) signals)
      :min-name-size min-name-size :relief relief
      :xalign xalign :yalign yalign
      :top-changer top-changer)))

(when (find-package :cl-fad)
  
  (defun file-name-getter (file)
    (if-use (when (pathname-name file)
	      (concatenate 'string 
			   (pathname-name file) "." (pathname-type file)))
	    (car(last (pathname-directory file)))))
  
  (defun make-directory-tiles
      (&optional (directory ".") &key
       (accept-hidden nil)
       (accept-hook
	(lambda (file)
	  (if accept-hidden t
	      (not(char= #\. (aref (file-name-getter file) 0))))))
       (name-hook
	(lambda (file &optional is-dir)
	  (format nil (if is-dir "~D/" "~D") (file-name-getter file)))))
    "Makes tiles from a directory name, can subsequently be fed into\
 make-instance 'tiler :tiles"
    (mapcan
     (lambda (file)
       (cond
	 ((not (funcall accept-hook file)))	     
	 ((fad:directory-pathname-p file)
	  (let*((childs (make-directory-tiles file))
		(sum 0)
		(area (dolist (c childs sum) (setf- + sum (area c)))))
	    (list
	     (make-instance 'tile :name (funcall name-hook file t)
			    :child-tiles childs 
			    :area area))))
	 ((fad:file-exists-p file)
	  (let ((area (with-open-file (stream file :if-does-not-exist nil)
			(if stream (file-length stream) 0))))
	    (list
	     (make-instance 'tile :name (funcall name-hook file)
			    :area area))))))
     (fad:list-directory directory))))

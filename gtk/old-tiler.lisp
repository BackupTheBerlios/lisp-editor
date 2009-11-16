(in-package :cl)

(defpackage :gtk-tiler
  (:use :common-lisp :generic :denest
	:gtk-stuff)
  (:export tile
	   tiler changing-tiler full-tiler
	   tiles-total-area
	   tiles-align tiles-ratio-align tiles-dud-hook
	   button-tile
	   child-hook-wrap-frame
	   make-directory-tiles)
  (:documentation
   "Visualization of size of things by subdividing a rectangle."))

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

(defmethod area ((of list))
  (car of))

(defclass tiler (list-alignment)
  ((tiles :initarg :tiles :type list))
  (:metaclass gobject:gobject-class)
  (:documentation
   "Visualization of size of things by subdividing a rectangle."))

(defun tiles-total-area (tiles &key (area 0))
  "Calculates total area of tiles."
  (dolist (tile tiles area)
    (setf- + area (area tile))))

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
	(pos (if-use
	      (position-if (lambda (tile)
			     (not(unless (> (area tile) small-area)
				   (setf- + sum (area tile)))))
			   tiles)
	      0)))
    (if (= 0 pos)
      tiles
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

(defun button-tile
    (&key (min-name-size 30) (relief :normal) (xalign 5d-1) (yalign 5d-1)
          signals)
  "Makes a function to produce a button tile."
  (lambda (name area w h subtiles path)
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
	    (funcall (cadr s) name area w h subtiles path))))
      button)))

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
    ((tiler tiler) &key
     (tile-hook #'tiles-ratio-align)
 ;Width and height target, initial direction, and maximum depth.
     w h (depth 30) (dir :h) ;TODO align direction with frame dir!
 ;Total area
     (total-area (tiles-total-area (slot-value tiler 'tiles)))
 ;Small area is the area smaller then which no childs are to be displayed.
 ;Small ratio is used to calculate small-area with total-area
     (small-ratio 0.02) (small-area (* small-ratio total-area))
 ;Too-small ratio is for non-childs that are too small, and are to me
 ;put together under 'small' button.
     (too-small-ratio 0.01) 
     (too-small-area (* too-small-ratio total-area))
 ;Make-tile produces the tile, min-name-size and button-tile-signals
 ;entered in button-tile, as default.
     (min-name-size 30) button-tile-signals
     (make-tile (button-tile :min-name-size min-name-size
			     :signals button-tile-signals))
 ;Top tile, defaultly same as make-tile.
     (make-top-tile make-tile)
     (child-hook (lambda (tiler tile path) tiler))
 ;Path
     path)
  (with-slots (tiles) tiler
    (flet ((w-h (area)
	     (case dir
	       (:v (values w (floor (* h (/ area total-area)))))
	       (:h (values (floor (* w (/ area total-area))) h))))
	   (mk-tiler (area nw nh subtiles &optional push-path)
	     "Need to pass all the damn arguments on."
	     (make-instance 'tiler :tiles subtiles
	       :w nw :h nh :depth (- depth 1)
	       :dir (case dir (:v :h) (:h :v))
	       :total-area area
	       :small-area small-area :too-small-area too-small-area
	       :make-tile make-tile :make-top-tile make-top-tile
	       :path (cons push-path path))))
      (setf (widgets tiler)
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
		      (with-slots (name fn child-tiles) (car subtiles)
			(funcall (if (null child-tiles)
				     (if-use fn make-tile) make-top-tile)
				 name area nw nh nil path)))
		     ((< area too-small-area)
		      (funcall make-top-tile
			       (format nil "small~%(~D)" (length subtiles))
			       area nw nh subtiles path))
		     (t
		      (mk-tiler area nw nh subtiles)))))
		 (tile
		  (denest
		   (with-slots (name area fn child-tiles) tile)
		   (multiple-value-bind (nw nh) (w-h area))
		   (unless (or (= nw 0) (= nh 0)))
		   (list)
		   (cond
		     ((null child-tiles)
		      (funcall (if-use fn make-tile)
			       name area nw nh nil path))
		     ((< area too-small-area)
		      (funcall make-top-tile
			       name area nw nh child-tiles path))
		     ((or (< area small-area) (< depth 0))
		      (funcall make-top-tile
			       name area nw nh child-tiles path))
		     (t
		      (funcall child-hook
			       (mk-tiler area nw nh child-tiles)
			       tile path)))))))
	     (funcall tile-hook tiles
		      :dir dir :w w :h h :total-area total-area
		      :path path)))))
  tiler)

(defclass changing-tiler (list-alignment) ;TODO messy..
  ((path :initform nil :initarg :path :type list)
   (tiles :initarg :tiles :type list
	  :documentation "First set of tiles that are given.")
   (mk-tiler :type function))
  (:metaclass gobject:gobject-class))

(defun follow-path (tiles path)
  (if (or (null tiles) (null path)) tiles
    (follow-path
     (dolist (tile tiles)
       (when (string= (car path) (slot-value tile 'name))
	 (return tile)))
     (cdr path))))

(defun set-tiler (ct subtiles)
  (with-slots (path tiles mk-tiler) ct
    (setf (widget ct)
	  (funcall mk-tiler subtiles
		   (lambda (name area w h subtiles new-path)
		     (declare (ignore name w h path))
		     (when subtiles
		       (setf path new-path)
		       (set-tiler ct subtiles))
		     nil)))))

(defun default-tiler (w h)
  (lambda (subtiles change-to-list)
    (make-instance 'tiler :tiles subtiles :w w :h h
      :make-tile (button-tile 
		  :signals (list (list "clicked" change-to-list))))))

(defmethod initialize-instance :after
    ((ct changing-tiler)
     &key w h (make-tiler (default-tiler w h)))
  (with-slots (path tiles mk-tiler) ct
    (setf mk-tiler make-tiler)
    (if-let want-tiles (follow-path tiles path)
      (set-tiler ct want-tiles) (set-tiler ct tiles)))
  ct)

(defclass full-tiler (gtk:alignment)
  ((box :initform (make-instance 'gtk:v-box) :type gtk:v-box)
   (path-list :initform (make-instance 'list-alignment)
	      :type list-alignment)
   (ct  :type changing-tiler))
  (:metaclass gobject:gobject-class))

(defmethod initialize-instance :after
    ((ft full-tiler)
     &key tiles w h (make-tiler (default-tiler w h)))
  (with-slots (box path-list ct) ft
    (setf ct (make-instance 'changing-tiler
			    :tiles tiles :make-tiler make-tiler))
    (with-slots (path tiles) ct
      (labels
	  ((make-path-buttons (p)
	     (if (null p) nil
		 (let ((button (make-instance 'gtk:button :label (car p))))
		   (gobject:g-signal-connect button "clicked"
		     (lambda (&rest stuff)
		       (declare (ignore stuff))
		       (set-tiler ct (follow-path tiles p))
		       (make-path-list)
		       nil))
		   (cons button (make-path-buttons (cdr p))))))
	   (make-path-list ()
	     (setf (widget path-list)
		   (make-instance 'list-alignment :dir :h
				  :widgets (make-path-buttons path)))))
	(gtk:container-add ft box)
	(gtk:box-pack-start box path-list)
	(gtk:box-pack-start box ct))))
  ft)

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
	(lambda (file area &optional is-dir)
	  (format nil (if is-dir "~D/~%~D" "~D~%~D")
		  (file-name-getter file) area))))
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
	     (make-instance 'tile :name (funcall name-hook file area t)
			    :child-tiles childs 
			    :area area))))
	 ((fad:file-exists-p file)
	  (let ((area (with-open-file (stream file :if-does-not-exist nil)
			(if stream (file-length stream) 0))))
	    (list
	     (make-instance 'tile :name (funcall name-hook file area)
			    :area area))))))
     (fad:list-directory directory))))
(in-package :cl)

(defpackage :gtk-tiler-test
  (:use :common-lisp :generic :gtk-tiler :gtk-stuff))

(in-package :gtk-tiler-test)
  
(defun 2k-tiles (&key (upto 10) (pwr 1.3))
  (denest:denest
   (denest:collecting ())
   (dotimes (k upto)
     (denest:collecting
	 (make-instance 'tile :name
			(format nil "~D" k) :area (expt pwr k))))))

(gtk:within-main-loop
  (gtk:widget-show 
   (make-instance 'gtk-stuff:list-window
     :widget  (make-instance 'tiler :w 1000 :h 1000 :depth 10
;        :tile-hook (curry #'tiles-align :small-ratio 0.1)
			    :tiles (2k-tiles :upto 30)))))

(defun show-tiles (tiles)
  (gtk:within-main-loop
    (gtk:widget-show 
     (make-instance 'list-window
       :widget (make-instance 'button-pathed-tiler :too-small 0.0 :small 0.0
			      :w 1000 :h 1000 :tiles tiles)))))


(show-tiles (2k-tiles :upto 30))

(show-tiles (make-directory-tiles "exemplars/image-magick"))
(show-tiles (make-directory-tiles))

(show-tiles (make-directory-tiles "/home/jasper/proj"))

(gtk-tiler:tiles-equal (make-directory-tiles) (make-directory-tiles))
  
(trace gtk-tiler:deduce-path)

(make-instance 'tiler :w 1000 :h 1000 :tiles nil
	       :tile-hook #'tiles-align)

(do-symbols (sym (find-package :gobject))
  (print sym))

(defun show-tiles-old (tiles)
  "Old way to do show-tiles"
  (gtk:within-main-loop
    (let ((width 1000) (height 1000)
	  (cw    (make-instance 'list-alignment)))
      (labels ((set-tiler (subtiles)
		 (setf (widget cw)
		       (make-instance 'tiler :dir :v :w width :h height
			 :too-small-ratio 0.01
			 :depth 10
			 :make-top-tile (button-tile 
				     :signals (list (list "clicked"
							  #'respond)))
			 :tiles subtiles)))
	       (respond (name area w h subtiles)
		 (declare (ignore name w h))
		 (when subtiles
		   (set-tiler subtiles))
		 nil))
	(set-tiler tiles)
	(gtk:widget-show 
	 (make-instance 'gtk-stuff:list-window
			:widgets (list cw)))))))


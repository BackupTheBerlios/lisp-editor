
(defpackage #:down-graph-draw
  (:use #:common-lisp #:iterate #:ltk #:down-graph)
  (:export arcs-for-links ltk-by-link-hor 
	   optimize-collected-by-dist ltk-draw-collected-by-dist))

(in-package #:down-graph-draw)

(defun true-fun (&rest args)
  (declare (ignore args))
  t)

(defun sqr (x) (* x x))
(defun delist (x) (if (listp x) (car x) x))

(defun arc (canvas fx tx y max-y &key (n 10))
  "Draws an arc. (The one ltk provides doesn't seem to work."
  (let ((dx (/ (- tx fx) n 1.0)))
    (flet ((y (x)
	     (let*((r (- (/ (- x fx) (- tx fx) 0.5) 1))
		   (h (sqr r)))
	       (+ (* h y) (* (- 1  h) max-y)))))
      (create-line
       canvas
       (do ((x fx (+ x dx))
	    (k 0 (+ k 1))
	    (out nil (append out (list x (y x)))))
	   ((>= k n)
	    (append out (list tx (y tx)))))))))

(defun arcs-for-links (canvas &key x y dx dy swap-odd (swap-dir t) under
		       predicate)
  "Arc for link as provided by #'do-by-link."
  (lambda (k j b el)
    (unless (or (= k j)
		(not (funcall predicate b))
		(not (funcall predicate el)))
      (multiple-value-bind (y dy)
	  (cond
	    (swap-odd (if (oddp k) (values (+ y under) dy) (values y (- dy))))
	    (swap-dir (if (> k j)  (values (+ y under) dy) (values y (- dy))))
	    (t        (+ y dy)))
	(arc canvas (+ x (* dx k)) (+ x (* dx j)) y (+ y dy))))))

(defun ltk-by-link-hor (between graph canvas
			&key (x 30) (y 30) (dx 30) (dy 10)
			(under 15) (predicate #'true-fun))
  "Draws links for horizontally placed elements."
  (iter (for b in between)
	(for k from 0)
	(when (funcall predicate b)
	  (create-text canvas (+ x (* k dx)) y (symbol-name (delist b)))))
  (do-by-link between between graph
	      (arcs-for-links canvas :x x :y y :dx dx :dy dy
			      :under under :predicate predicate)
	      :predicate predicate))

;TODO collect data how to draw first, then draw.
(defun ltk-draw-collected-by-dist
    (canvas at graph
     &key (x 30) (y 30) (step-y 50) (dx 30) (dy 10) (under 15)
     (predicate #'true-fun))
  "Draws collected-by-dbist data."
  (let ((from (car at)))
    (dolist (list at)
      (do-by-link from list graph
		  (lambda (k j fr to)
		    (when (and (funcall predicate fr)
			       (funcall predicate to))
		      (create-line* canvas
			(+ x (* dx k)) (- y step-y (- under))
			(+ x (* dx j)) y))))
      (ltk-by-link-hor
       list graph canvas
       :x x :y y :dx dx :dy dy :under under :predicate predicate)
      (setf from list
	    y (+ y step-y)))))

(defun optimize-collected-by-dist-1
    (at graph &key (factors '(1 1 1)) (predicate #'true-fun))
  "See #'optimize-collected-by-dist, except this one for one position.
TODO improve"
  (flet ((switched-better (pre i j)
	   "Returns whether swithing is better and score."
	   (cond ((funcall predicate (nth i (cadr at)))
		  (unless (funcall predicate (nth j (cadr at)))
		    (return-from switched-better t)))
		 ((funcall predicate (nth j (cadr at)))
		  (return-from switched-better t))
		 (t ;neither, don't care.
		  (return-from switched-better)))
	   (let ((score 0) (three (if (> (length pre) 3)
				      (subseq pre 0 3) pre))
		 (at (second pre)))
	     (flet ((keep-score (factor)
		      (lambda (k m x y)
			(declare (ignore x y))
			(let ((f (case k (0 1) (1 -1)
				       (t (error "Unexpected value.")))))
			  (setf score
				(+ score
				   (* factor f
				      (- (sqr (- m i)) (sqr (- m j))))))))))
	       (iter (for list in three)
		     (for f in factors)
		     (unless (= f 0)
		       (do-by-link (list (nth i at) (nth j at)) list
				   graph (keep-score f))
		       (do-by-link list (list (nth i at) (nth j at))
				   graph (lambda (k m x y)
					   (funcall (keep-score f)
						    m k y x)))))
	       (values (> score 0) score)))))
    (dotimes (i (length (cadr at)))
      (iter (for j from (+ i 1) to (- (length (cadr at)) 1))
	    (when (switched-better at i j)
	      (let ((tmp (nth j (cadr at))))
		(setf (nth j (cadr at)) (nth i (cadr at))
		      (nth i (cadr at)) tmp))))))
  at)

(defun optimize-collected-by-dist
    (at graph &key (factors '(1 1 0.4)) (predicate #'true-fun))
  "Somewhat optimizes collected-by-dist data to improve how it can be used\
 to represent the graph."
  (do ((iter at (cdr iter)))
      ((null (cdr iter)) at)
    (optimize-collected-by-dist-1 iter graph
				  :factors factors :predicate predicate)))

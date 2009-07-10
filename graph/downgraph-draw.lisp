(defpackage #:down-graph-draw
  (:use #:common-lisp #:iterate #:ltk #:down-graph)
  (:export get-rect-positions optimize-rect-by-dist rect-by-dist-drift
	   ltk-draw-from))

(in-package #:down-graph-draw)

(defun true-fun (&rest args)
  (declare (ignore args))
  t)

(defun sqr (x) (* x x))
(defun lensqr (x y) (+ (sqr x) (sqr y)))
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

(defclass text-rect ()
  ((symbol :initarg :symbol :type symbol)
   (text :initarg :text :type string)
   (x :initarg :x :type fixnum :reader x)
   (y :initarg :y :type fixnum :reader y)
   (w :initarg :w :type fixnum :reader w)
   (h :initarg :h :type fixnum :reader h)))

(defun subseq* (seq fr to)
  (if (< (length seq) to)
      (subseq seq fr to)
      (subseq seq fr)))

(defun make-text-rect (symbol x y &key (max-len 10) (max-lines 4)
		       (text (symbol-name symbol)) (char-w 10) (char-h 15))
  "Makes a text-rect, making the text shorter to have a nicer aspect ratio."
  ;TODO fix aspect ratio.
  (make-instance
   'text-rect :symbol symbol :text (symbol-name symbol)
   :x x :y y  :w (* char-w (length text)) :h (+ char-h 4)))


(defun get-rect-positions (by-dist-list &key (x 10) (y 10) (dx 20) (dy 50)
			   (predicate #'true-fun))
  "For a by-distance showing, optimize the postion."
  (declare (type list by-dist-list))
  (let ((tx 0) (ty 0))
    (values
     (iter (for list in by-dist-list)
	   (let ((x x))
	     (collect
		 (iter (for sym in list)
		       (when (funcall predicate sym)
			 (let ((rect (make-text-rect sym x y)))
			   (with-slots (w h) rect
			     (setf x (+ x w dx)
				   tx (max tx (+ x w))
				   ty (max ty (+ y h dy))))
			   (collect rect)))))
	     (setf y ty)))
     tx ty)))

(defun do-rect-connections (rect graph list fun
		&key (syms (cdr (assoc (slot-value rect 'symbol) graph))))
  "Do function for those in list connected from rect."
  (dolist (el list)
    (when (and (not(eq el rect)) (find (slot-value el 'symbol) syms))
      (funcall fun el))))

(defun optimize-rect-by-dist-1 (at graph &key (factors '(1 1 1)))
  "See #'optimize-collected-by-dist, except this one for one position.
TODO improve"
  (flet ((score (rect factor with &optional (px (x rect)) (py (y rect)))
	   (when (eq rect with)
	     (return-from score 0))
	   (let ((syms (cdr (assoc (slot-value rect 'symbol) graph))))
	     (iter (for el in with)
	       (with-slots (symbol x y) el
		 (when (find symbol syms)
		   (sum (* factor (lensqr (- x px) (- y py))))))))))
    (let (did)
      (do ((i (cadr at) (cdr i)))
	  ((null i) (values))
	(do ((j (cdr i) (cdr j)))
	    ((null j) (values))
	  (when (> (iter
		     (for el in (if (null (cdddr at)) at (subseq at 0 3)))
		     (for f in factors)
		     (sum (- (+ (score (car i) f el)
				(score (car j) f el))
			     (score (car i) f el
				    (x(car j)) (y(car j)))
			     (score (car j) f el
				    (x(car i)) (y(car i))))))
		   0)
	    (setf did t)
	    (let* ((tmp (car i)) (tmp-x (x tmp)) (tmp-y (y tmp)))
	      (setf (car i) (car j))
	      (setf (car j) tmp)
	      (with-slots (x y) (car j) ;Swap the two.
		(setf (slot-value (car i) 'x) x
		      (slot-value (car i) 'y) y
		      x tmp-x  y tmp-y))))))
      (values at did))))

(defun fix-pos (rect-list &key (dx 20))
  "Fixes horizontal positions."
  (do ((r rect-list (cdr r)))
      ((null (cdr r)) (values))
    (with-slots (x w) (car r)
      (setf (slot-value (cadr r) 'x) (+ x w dx)))))

(defun optimize-rect-by-dist
    (at graph &key (factors '(1 1 0.4)) (dx 40) (cnt 16))
  "Somewhat optimizes collected-by-dist data to improve how it can be used\
 to represent the graph."
  (dotimes (k cnt)
    (do ((iter at (cdr iter)))
	((null (cdr iter)) at)
      (fix-pos (car iter) :dx dx)
      (optimize-rect-by-dist-1 iter graph :factors factors))))

#| TODO move rectangles that last bit to get them aligned where possible.
 (defun rect-by-dist-drift (rect-by-dist graph)
  (do ((iter rect-by-dist (cdr iter)))
      ((null (cdr iter)) (values))
    (dolist (rect (car iter))
      (do-rect-connections rect graph (car list)
	 |#

(defun draw-text-default (canvas)
  "Default way of drawing text in ltk-draw-from."
  (lambda (rect)
    (with-slots (x y text) rect
      (create-text canvas x (+ y 2) text))))

(defun draw-vert-default (canvas)
  "Default way of drawing 'vertical' lines in ltk-draw-from."
  (lambda (from to)
    (create-line canvas
		 (append (with-slots (x y w h) from
			   (list (+ x (/ w 2)) (+ y h)))
			 (with-slots (x y w) to
			   (list (+ x (/ w 2)) y))))))

(defun draw-hor-default (canvas dy n)
  "Default way of drawing 'horizontal' lines in ltk-draw-from."
  (lambda (from to)
    (unless (eq from to)
      (let ((y (if (< (x from) (x to))
		 (max (+ (y from) (h from)) (+ (y to) (h to)))
		 (max (y from) (y to)))))
	(arc canvas (with-slots (x w) from (+ x (/ w 2)))
	     (with-slots (x w) to (+ x (/ w 2)))
	     y (if (< (x from) (x to)) (+ y dy) (- y dy)) :n n)))))

(defun ltk-draw-from
    (rect-by-dist graph &key canvas (dy 20) (n 10)
     (draw-text (draw-text-default canvas))
     (draw-vert (draw-vert-default canvas))
     (draw-hor (draw-hor-default canvas dy n)))
  "Draws by dist, output from get-rect-positions (better optimize first.)"
  (dolist (rect (car rect-by-dist))
    (do-rect-connections rect graph (cadr rect-by-dist)
      (lambda (el)
	(funcall draw-vert rect el))))
  (do ((i rect-by-dist (cdr i)))
       ((null (cdr i)) (values))
    (dolist (rect (cadr i))
      ;TODO memoize :syms keyword of do-rect-connections.
      (do-rect-connections rect graph (car i)
        (lambda (el)
	  (funcall draw-vert el rect)))
      (unless (null(cddr i))
	(do-rect-connections rect graph (caddr i)
	  (lambda (el)
	    (funcall draw-vert rect el))))
      (do-rect-connections rect graph (cadr i)
	  (lambda (el)
	    (funcall draw-hor rect el)))))
  (dolist (list rect-by-dist)
    (dolist (rect list)
      (funcall draw-text rect))))

#|

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


|#

(defpackage :test
  (:use :common-lisp :down-graph :down-graph-draw :ltk :code-scan))
(in-package :test)

(do-by-dist '(a) (by-link '((a b c) (b a d e) (c q d)
			    (q r) (r s) (s t)))
	    5)

(let ((sg '((a b c) (b a d e) (c q d a))))
  (by-link sg)
  (by-symbol sg)
  (by-symbol (by-link sg)))

(let ((between '(a b d)))
  (do-by-link between between '((a b c) (b a d e) (c q d a) (d b))
	      (lambda (k j x y) (print (list k j x y)))))

(collect-by-dist '(a) '((a b c) (b c d e) (c q d)
			(q r) (r s) (s t)) 4)

(reverse-graph '((a b c) (b c d e) (c q d)
			(q r) (r s) (s t)))

(let*((graph '((a b c d) (b d)))
      (lect (collect-by-dist '(a) graph 4)))
  (format nil "~{~a~%~}~2%~{~a~%~}" lect
	  (optimize-collected-by-dist (mapcar #'copy-list lect) graph)))


(with-ltk ()
  (let ((canvas (make-instance 'canvas)))
    (pack canvas)
    (ltk-by-link-hor '(a b d) '((a b c d) (b d) (c q d a) (d a b)) canvas)))

(defun ltk-by-dist (from graph depth canvas
		    &key (x 30) (y 30) (step-y 50) (dx 30) (dy 10)
		         (under 15))
  (do-by-dist from graph depth
      (lambda (list)
	(do-by-link from list graph
	  (lambda (k j fr to)
	    (create-line* canvas
			  (+ x (* dx k)) (- y step-y (- under))
			  (+ x (* dx j)) y)))
	(ltk-by-link list graph canvas
	   :x x :y y :dx dx :dy dy :under under)
	(setf from list
	      y (+ y step-y)))))

;	   (let ((a 'aaaaaaaa) (b 'bbbbbbbbb) (c 'cccccccccc)
;		 (d 'dddddddd) (e 'eeeeeeeee) (q 'qqqqqqqqqq)
;		 (r 'rrrrrrrr) (s 'sssssssss))
;	     (list (list a b c d e) (list b e c) (list d q r)
;		   (list q r e) (list r s) (list s t))))
;	   (to-tree '(+ a (- b c) (/ d (* e (sqr f))))))
;	   '((a b c d e) (b e c) (d q r)
;	     (q r e) (r s) (s t)))

(defun ltk-draw (from graph depth
		 &key (canvas (make-instance 'canvas))
		      (x 30) (y 30) (dx 30) (dy 30) (step-y 60)
		 (predicate (lambda(el) t)))
  (with-ltk  ()
    (pack canvas)
    (let ((lect (collect-by-dist from graph depth)))
;      (ltk-draw-collected-by-dist canvas lect graph); :dx 110 :step-y 60 :dy 20)
      (ltk-draw-collected-by-dist
       canvas
       (optimize-collected-by-dist (mapcar #'copy-list lect) graph
				   :predicate predicate)
       graph :x x :y y :dx dx :dy dy :step-y step-y
       :predicate predicate))))
						      

(ltk-draw '(a) '((a b c d e) (b e c) (d q r)
		 (q r e) (r s) (s t)) 4)

(ltk-draw '(sumsqr) (car code-scan:*function-links*) 4 :dx 120
	  :canvas (make-instance 'canvas :width 1300 :height 1000))

(ltk-draw '(ltk-draw-collected-by) (car code-scan:*function-links*)
	  4 :canvas (make-instance 'canvas :width 1300 :height 1000)
	    :dx 120)

(ltk-draw '(code-scan:scanning-macrohook) (car code-scan:*function-links*)
	  4 :canvas (make-instance 'canvas :width 1300 :height 1000)
	    :dx 30
	    :predicate
	    (lambda (el)
	      (or (string= (package-name (symbol-package el)) "CODE-SCAN")
		  (string= (package-name (symbol-package el)) "DOWN-GRAPH"))))
	      

(assoc 'code-scan:scanning-macrohook (car *function-links*))
;    (ltk-by-dist '(a) '((a b c) (b c d e) (c q d)
;			(q r) (r s) (s t)) 4
;		 canvas)))

(let ((between '(a b d)))
  (do-by-link between between '((a b c) (b a d e) (c q d a) (d b))
	      (lambda (k j x y) (print (list k j x y)))))

(mapcar #'print (car *function-links*))
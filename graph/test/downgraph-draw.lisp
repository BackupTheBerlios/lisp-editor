
(defpackage :testing
  (:use :common-lisp :ltk :down-graph :down-graph-draw :code-scan))

(in-package :testing)

(defun draw-function-links (of-fun &key canvas)
  "Draws the function links for a function.
TODO probably useful for more then just this small test."
  (ltk-draw-from (list (car(assoc of-fun (car *function-links*))))
		 (car *function-links*) :canvas canvas
		 :predicate (lambda (el)
			      (let ((pkg (package-name(symbol-package el))))
				(and (not(string= pkg "COMMON-LISP"))
				     (not(string= pkg "SB-KERNEL")))))
		 ))

;;NOTE if not scanned yet, no data on it!!
(with-ltk ()
  (let ((canvas (make-instance 'canvas :width 1000 :height 1000)))
    (pack canvas)
    (draw-function-links 'autodoc:autodoc :canvas canvas)))

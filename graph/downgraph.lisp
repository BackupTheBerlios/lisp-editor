(defpackage :down-graph
  (:use :common-lisp :generic :iterate)
  (:export add-link list-by-dist))

(in-package :down-graph)

(defun add-link (graph from to)
  "Adds a link to a graph."
  (declare (type (cons list null) graph)
	   (type symbol from to))
  (if-use
   (when-let got (assoc from (car graph))
    (if (find to (cdr got)) t
      (push to (cdr got))))
   (push (list from to) (car graph))))

(defun list-by-dist (from graph depth &key not)
  "List ordered by the distance from the from list."
  (declare (type list from graph)
	   (type fixnum depth))
  (when (> depth 0)
    (let ((result nil))
      (dolist (el from)
	(when-let assoc (assoc el graph)
	  (dolist (a (cdr assoc))
	    (unless (find a not)
	      (push a not)
	      (push a result)))))
      (cons from
	    (list-by-dist result graph (- depth 1) :all all)))))

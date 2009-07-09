
(defpackage #:down-graph
  (:use #:common-lisp #:iterate #:ltk)
  (:export by-symbol by-link with-by-symbol
	   add-link to-tree reverse-graph
	   do-by-dist do-by-link collect-by-dist
	   optimized-collected-by-dist))

(in-package #:down-graph)


(defun delist (x)
  (if (listp x) (car x) x))

(defmacro dolist* ((el list) &body body)
  (let ((i (gensym)))
    `(do ((,i ,list (cdr ,i)))
	 ((null ,i) nil)
       (symbol-macrolet ((,el (car ,i)))
	 ,@body))))

(defun by-symbol (link-graph)
  "Destructively makes references to other elements in the graph by symbol."
  (dolist* (el link-graph)
    (dolist* (link (cdr el))
      (setf link (car link))))
  link-graph)

(defun add-link (list from to)
  (let ((assoc (assoc from (car list))))
    (if assoc 
      (unless (dolist (el (cdr assoc))
		(when (eql el to) (return t)))
	(push to (cdr assoc)))
      (push (list from to) (car list)))))

(defun to-tree (x)
  (cons (cons (car x) (iter (for el in (cdr x))
			    (collect (delist el))))
	(iter (for el in (cdr x))
	      (when (listp el)
		(appending (to-tree el))))))

(defun by-link (symbol-graph)
  "Destructively onverts graph referenced by symbols to one by links.
Returns nil. WARNING you _cannot_ print them (nor can your editor)"
  (dolist* (el symbol-graph)
    (dolist* (link (cdr el))
      (let ((assoc (assoc link symbol-graph)))
	(if (null assoc)
	  (progn (push (list link) (cdr symbol-graph))
		 (setf link (cadr symbol-graph)))
	  (setf link assoc)))))
  symbol-graph)

(defmacro with-by-symbol (link-graph &body body)
  `(progn
     (by-symbol ,link-graph)
     (let ((out (progn ,@body))) ;Can't access out.
       (by-link ,link-graph)
       out)))

(defun reverse-graph (graph)
  "Reverses all directions on the graph."
  (iter (for element in graph)
	(collect (cons (car element)
		       (iter (for el in graph)
			     (when (dolist (s-el (cdr el))
				     (when (eql (car element) s-el)
				       (return t)))
			       (collect (car el))))))))

(defun print-fn (list &optional (stream t))
  "Can print the elements in both symbol and link notation."
  (dolist (fr list)
    (format stream "~D  " (delist fr)))
  (format stream "~%"))

(defun do-by-dist
    (from graph depth &optional (do-fn #'print-fn)
     (done (iter (for el in from)
		 (appending (list el t)))))
  "Partially prints a graph, different lines representing different\
 distances from one of the 'from'."
  (when (or (<= depth 0) (null from))
    (return-from do-by-dist))
  (funcall do-fn from)
  (let ((from (iter (for fr in from)
		    (dolist (el (if (listp fr) fr (cdr (assoc fr graph))))
		      (let ((sym (delist el)))
			(unless (getf done sym)
			  (setf done (append done (list sym t)))
			  (collect el)))))))
    (do-by-dist from graph (- depth 1) do-fn done)))

(defun true-fun (&rest args)
  (declare (ignore args))
  t)

(defun do-by-link (from to graph do-fn &key (predicate #'true-fun))
  "Does do-fn for different links from to.
Predicate works only on 3rd argument, you have to ignore others yourself."
  (flet ((in-between (el)
	   (iter (for b in to)
		 (for k from 0)
		 (when (eql (delist el) (delist b))
		   (return k)))))
    (iter (for k from 0)
	  (for b in from)
	  (when (funcall predicate b)
	    (let ((graph-el (if (listp b) b (assoc b graph))))
	      (dolist (el (cdr graph-el))
		(let ((j (in-between el)))
		  (when j
		    (funcall do-fn k j b el)))))))))

(defun sqr (x) (* x x))

(defun collect-by-dist (from graph depth)
  "Collects elements based on distance from FROM, up to depth."
  (iter
    (do-by-dist from graph depth (lambda (list) (collect list)))
    (finish)))

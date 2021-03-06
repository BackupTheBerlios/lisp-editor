;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :graph-scanned
  (:use :common-lisp :alexandria :package-stuff
	:cl-dot :expression-scan :gil-autodoc)
  (:export )
  (:documentation 
   "Uses cl-dot to graph the objects.
NOTE still in development, some 'playing' in it.
TODO: 
* Determine if connections are to be expected depending on context.
* work it into the autodoc"))

(in-package :graph-scanned)
 
(def-document :title-short (object &key)
  (let ((name (gil-autodoc::give-name object)))
    (nsubstitute #\Newline #\-
		 (string-downcase
		  (subseq name (+ (position #\_ name :from-end t) 1))))))

(defun boring-link (sym in-package)
  "Whether a link is uninteresting. (As-in water under the bridge,)"
  (case in-package
    ((:generic :denest) ;Generic is interesting inside.
     nil)
    (t ;;Generic is always boring seen from outside.
     (case (package-keyword sym)
       (:generic t) (:denest t) (:alexandria)))))

(defun when-access (defined-by in-package)
  (lambda (sym)
    (mapcan (lambda (def)
	      (when-let (got (access-result def sym))
		(unless (boring-link sym in-package)
		  (list got))))
	    defined-by)))

(defun dep-link (depend in-package)
  (if-let (overrider (slot-value depend 'expr-scan::overrider))
    (dep-link overrider in-package)
    (with-slots (fun-dep var-dep) depend
      (append (mapcan (when-access '(defun defmacro defgeneric) in-package)
		      fun-dep)
	      (mapcan (when-access '(defvar defparameter) in-package) 
		      var-dep)))))

(defun direct-connection-p (graph from to)
  "If exists, finds connection between two nodes."
  (find-if (curry #'eq to) (graph-object-points-to graph from)))

;;Graphing fully; connections between functions.

(defmethod graph-object-node ((graph (eql 'full)) (null null)))

(defmethod graph-object-node ((graph (eql 'full)) object)
  (make-instance 'node
    :attributes `(:label ,(document :title-short object))))

(defmethod graph-object-node
    ((graph (eql 'full)) (method track-method))
  (make-instance 'node :attributes '(:label ".")))

(defmethod graph-object-points-to
    ((graph (eql 'full)) (track track-form))
  (destructuring-bind (definer name &rest assoc)
      (slot-value track 'expr-scan::form)
    (let ((pkg (package-keyword name)))
      (case definer
	(defpackage
	 (flet ((coerce-package (sym &key (package (find-package name)))
		  (intern (symbol-name sym) package)))
	   (append
	    (mapcan (when-access '(defun defmacro defvar defparameter) pkg)
		    (mapcar #'coerce-package (cdr(assoc :export assoc))))
	     ;Note: assumes stuff on how declared.
	    (mapcan (when-access '(defpackage) pkg)
		    (mapcar #'package-keyword (cdr(assoc :use assoc)))))))
	(defgeneric
	 (dep-link track pkg))))))

(defmethod graph-object-points-to ((graph (eql 'full)) (fun track-fun))
  (dep-link fun (package-keyword (name fun))))

;;TODO recognize identically linked methods.
(defmethod graph-object-points-to
    ((graph (eql 'full)) (fun track-generic))
  (append (dep-link fun (package-keyword (name fun)))
	  (slot-value fun 'expr-scan::methods)))

(defmethod graph-object-points-to
    ((graph (eql 'full)) (method track-method))
  (dep-link method (package-keyword (name method))))

;Graphing just connections between packages.
(defmethod graph-object-node ((graph (eql 'package)) (null null)))
(defmethod graph-object-node ((graph (eql 'package)) object)
  (make-instance 'node
    :attributes `(:label ,(document :title-short object))))

(defmethod graph-object-points-to
    ((graph (eql 'package)) (track track-form))
  (destructuring-bind (definer name &rest assoc)
      (slot-value track 'expr-scan::form)
    (declare (ignore name))
    (case definer
      (defpackage
       ;Note: assumes stuff on how declared.
	 (mapcan
	  (lambda (pkg)
	    (when-let (got (access-result 'defpackage 
					  (package-keyword pkg)))
	      (list got)))
	  (cdr(assoc :use assoc)))))))

(defun minimal-root (graph elements)
  "List everything it points to but _not_ indirectly."
  (unless (null elements)
    (let ((links (graph-object-points-to graph (car elements))))
      (cons (car elements)
	    (minimal-root
	     graph (remove-if (lambda (el)
				(find-if (rcurry #'eq el) links))
			      (cdr elements)))))))

(defmethod graph-object-points-to ((graph (eql 'full-not-indirect)) track)
  (minimal-root graph (graph-object-points-to 'full track)))

(defmethod graph-object-node ((graph (eql 'full-not-indirect)) track)
  (graph-object-node 'full track))

;(defmethod graph-object-points-to ((graph (eql 'full-no-via)) object)
;  (remove-if (curry #'connection-p '
;(graph-object 'full object)

;TODO graph omiting links that are also already ind

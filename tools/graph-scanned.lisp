;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
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

(defmethod graph-object-node ((graph (eql 'graph-scanned)) object)
  (make-instance 'node
    :attributes `(:label ,(document :title-short object))))

(defmethod graph-object-node
    ((graph (eql 'graph-scanned)) (method track-method))
  (make-instance 'node :attributes '(:label ".")))


(defun package-keyword (sym)
  (intern (to-package-name sym) :keyword))

(defun boring-link (sym in-package)
  "Whether a link is uninteresting. (As-in water under the bridge,)"
  (case in-package
    ((:generic :denest) ;Generic is interesting inside.
     nil)
    (t ;;Generic is always boring seen from outside.
     (case (package-keyword sym)
       (:generic t) (:denest t)))))

(defun when-access (defined-by in-package)
  (lambda (sym)
    (mapcan (lambda (def)
	      (when-let (got (access-result def sym))
		(unless (boring-link sym in-package)
		  (list got))))
	    defined-by)))

(defun dep-link (depend in-package)
  (with-slots (fun-dep var-dep) depend
    (append (mapcan (when-access '(defun defmacro defgeneric) in-package)
		    fun-dep)
	    (mapcan (when-access '(defvar defparameter) in-package) 
		    var-dep))))

(defmethod graph-object-points-to
    ((graph (eql 'graph-scanned)) (track track-form))
  (destructuring-bind (definer name &rest assoc)
      (slot-value track 'expr-scan::form)
    (let ((pkg (package-keyword name)))
      (case definer
	(defpackage
	 (flet ((coerce-package (sym)
		  (intern (symbol-name sym) (find-package name))))
	   (append
	    (mapcan (when-access '(defun defmacro defvar defparameter) pkg)
		    (mapcar #'coerce-package (cdr(assoc :export assoc))))
	     ;Note: assumes stuff on how declared.
	    (mapcan (when-access '(defpackage) pkg)
		    (mapcar #'coerce-package (cdr(assoc :use assoc)))))))
	(defgeneric
	 (dep-link track pkg))))))

(defmethod graph-object-points-to
    ((graph (eql 'graph-scanned)) (fun track-fun))
  (dep-link fun (package-keyword (name fun))))

(defmethod graph-object-points-to
    ((graph (eql 'graph-scanned)) (fun track-generic))
  (append (dep-link fun (package-keyword (name fun)))
	  (slot-value fun 'expr-scan::methods)))

(defmethod graph-object-points-to
    ((graph (eql 'graph-scanned)) (method track-method))
  (dep-link method (package-keyword (name method))))

(let ((dgraph
       (generate-graph-from-roots
	'graph-scanned
	(graph-object-points-to
	 'graph-scanned (access-result 'defpackage :cl-fad)))))
  (cl-dot:dot-graph dgraph "/home/jasper/proj/test.png" :format :png))
;  (cl-graph-object-points-to
;   'graph-scanned (access-result 'defpackage :generic)))

(time
 (let*((root
       (mapcan
	(lambda (pkg)
	  (graph-object-points-to 'graph-scanned (access-result 'defpackage pkg)))
	'(:generic :denest
	  :package-stuff :expression-hook :expression-scan
	  
	  :gil :gil-vars :gil-share :gil-style
	  :gil-read :gil-info :gil-user
	  :gil-output-util :gil-html :gil-txt :gil-latex
	  
	  :gil-contents :gil-log
	  
	  :gil-autodoc)))
      (dgraph
       (generate-graph-from-roots 'graph-scanned root)))
  (cl-dot:dot-graph dgraph "/home/jasper/proj/lisp-editor.png"
		    :format :png)))
 

;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :scanned-searching
  (:use :common-lisp :alexandria :generic :denest
	:package-stuff :path-stuff
	:expression-hook :expression-scan)
  (:export get-external-objects get-internal-objects
	   list-packages-below-path)
  (:documentation "Package containing functions to search in the scanned\
 functions of expr-scan."))

(in-package :expression-scan)

(defun select-name (name)
  (lambda (track) (eql (name track) name)))

(defun package-scan-list (pkg &optional (select (constant t)))
  "Read the scan-list of a package."
  (typecase pkg
    (symbol
     (package-scan-list (package-keyword pkg) select))
    (track-package
     (remove-if-not select (slot-value pkg 'expr-scan::scan-list)))))

;;TODO what to do with these four?
(defun sort-objects (list &optional (sort-compare :default))
  "Sorts objects."
  (sort list
	(case sort-compare
	  (:default
	   (lambda (a b)
	     (or (string> (expr-scan::etype a) (expr-scan::etype b))
		 (string> (expr-scan::name a) (expr-scan::name b)))))
	  (t
	   sort-compare))))

(defun get-objects-of-sym (sym &optional (by-names *by-names*) list)
  (dolist (by-name by-names list) ;Collect externals.
    (when-let (res (access-result by-name sym))
      (push res list))))

(defun get-external-objects (package &optional (sort-compare :default))
  "Gets sorted external objects of package."
  (let (list)
    (do-external-symbols (sym package)
      (when (same-package sym package)
	(gen:setf- append list (get-objects-of-sym sym))))
    (sort-objects list sort-compare)))

(defun get-internal-objects (package &optional (sort-compare :default))
  (let (list)
    (do-symbols (sym package)
      (when (and (same-package sym package) (not (external-p sym)))
	(gen:setf- append list (get-objects-of-sym sym))))
    (sort-objects list sort-compare)))

;;
(defun list-packages-below-path (path)
  "Lists all the packages that are in the path or in subdirectories\
 thereof."
  (remove-if-not
   (lambda (track)
     (with-slots (paths) track
       (find-if (compose (curry #'sub-path-of path) #'namestring) paths)))
   (copy-list *package-list*)))

(defun list-scanned-in-package (pkg under-name)
  "List the scanned forms under the given name from that package."
  (slot-value (access-result 'defpackage pkg)

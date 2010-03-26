;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :common-lisp)

(defpackage :package-stuff
  (:use :common-lisp :alexandria)
  (:export to-package to-package-name same-package symbol-package> 
	   listfind-package package-keyword)
  (:documentation "Some stuff to do with packages."))

(in-package :package-stuff)

(defun to-package (x)
  "Gets the package of x."
  (cond	((or (keywordp x) (stringp x))
	 (find-package x))
	((symbolp x)
	 (symbol-package x))
	((packagep x)
	 x)))

(defun to-package-name (x)
  "Gets the package name from the x."
  (when-let (pkg (to-package x))
    (package-name pkg)))

(declaim (inline to-package-name))

(defun same-package (a b)
  "Whether a and b are the same package."
  (string= (to-package-name a) (to-package-name b)))

(defun symbol-package> (a b)
  "Compares symbols by comparing their package names with string>,\
 when string= the symbol-names are compared."
  (let ((pkg-a (to-package-name a))
	(pkg-b (to-package-name b)))
    (cond ((string> pkg-a pkg-b) t)
	  ((string= pkg-a pkg-b)
	   (string> (symbol-name a) (symbol-name b))))))

(defun listfind-package (symbol package-list)
  "If the package is in the symbol, returns that package."
  (when (find-if (lambda (el)
		   (same-package (if (packagep el) el (find-package el))
				 symbol))
		 package-list)
    (symbol-package symbol)))

(defun package-keyword (sym)
  (when-let (package-name (when sym (to-package-name sym)))
    (intern package-name :keyword)))

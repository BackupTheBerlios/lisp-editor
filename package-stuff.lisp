(cl:in-package :common-lisp)

(defpackage :package-stuff
  (:use :common-lisp :generic)
  (:export to-package-name same-package symbol-package> listfind-package)
  (:documentation "Some stuff to do with packages."))

(in-package :package-stuff)

(defun to-package-name (x)
  "Gets the package name from the x."
  (cond ((packagep x) (package-name x))
	((symbolp x)  (package-name (symbol-package x)))
	((stringp x)  x)))

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

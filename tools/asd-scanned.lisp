;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :asd-scanned
  (:use :common-lisp :alexandria :generic :expression-scan
	:package-stuff :path-stuff)
  (:export *system-name-hook* asd-scanned)
  (:documentation 
   "Automatically make .asd files from scan results and given packages\
 to do so on."))

(in-package :asd-scanned)

(defvar *system-name-hook* #'identity
  "Function called on package name to get system name.")

(defun asd-scanned-1 (package stream to-path)
  "Scan a single package."
  (let*((by-name (intern* (to-package-name package) :keyword))
	(track (access-result 'defpackage by-name)))
    (assert track nil "Could not find scan result of ~a" by-name)
    (format stream "~2%  (defsystem :~a"
	    (string-downcase(funcall *system-name-hook* by-name)))
    (let ((assoc (cddr (slot-value track 'expr-scan::form)))
	  (files (reverse (copy-list
			   (slot-value track 'expr-scan::paths)))))
      (flet ((obtain (name)
	       (cdr (assoc name assoc))))
	(when-let (descr (car (obtain :documentation)))
	  (format stream "~%    :description ~s" descr))
	(format stream "~%    :serial t")
	(when-let (uses (obtain :use))
	  (format stream " ~%    :depends-on (~{:~a ~})"
		  (mapcar (lambda (use)
			    (string-downcase
			     (funcall *system-name-hook* use)))
			  uses)))
       ;And now to list the files: 
 ;(Not too good, dumbly makes a module for each file in directory.)
	(format stream "~%    :components (")
	(dolist (file files)
	  (format stream "~%       ")
	  (let ((path (from-path-root file to-path)))
	    (do ((k 0 (+ k 1))
		 (j -1 j))
		((>= k (length path)) nil)
	      (when (char= (aref path k) #\/)
		(format stream "(:module ~s " (subseq path (+ j 1) k))
		(setq j k)))
	    (format stream "(:file ~s)"
		    (subseq file (+ (position #\/ file :from-end t) 1)))
	    (dotimes (k (path-count-directory-depth path))
	      (format stream ")"))))
	(format stream "~%    ))")))))
			*default-pathname-defaults*))

(defun asd-scanned
    (packages &key (to-path *default-pathname-defaults*)
     (to-file "systems.asd") (if-exists :supersede))
  "Makes asd file of tracked package. Must be scanned first of course!"
  (let ((first (or (probe-file to-file) (eql if-exists :supersede))))
    (with-open-file (stream to-file :direction :output
		     :if-exists if-exists :if-does-not-exist :create)
      (when first
	(format stream "~%(in-package :cl-user)
\ (defpackage :auto-asd-scanned (:use :common-lisp :asdf))
\ (in-package :auto-asd-scanned)"))
      (mapcar (rcurry #'asd-scanned-1 stream to-path) packages))))

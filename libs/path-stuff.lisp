;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :path-stuff
  (:use :common-lisp :alexandria)
  (:export *path-root* from-path-root to-absolute 
	   path-count-directory-depth
	   file-extension)
  (:documentation "Some stuff to assist with paths and paths."))

(in-package :path-stuff)

(defvar *path-root* nil
  "What is currently 'root' in pathsystem.")
(declaim (type string *path-root*))

(defun from-path-root
    (path &optional 
     (path-root (namestring *default-pathname-defaults*)))
  "Translates to what the path would be named if . was path-root."
  (declare (type string path path-root))
  (let ((i (position #\/ path-root)))
    (declare (type (or null fixnum) i))
    (cond
      ((or (not i) (>= i (length path)))
       (from-path-root-backward path path-root))
      ((= i 0)
       (if (char= (aref path 0) #\/)
	 (from-path-root (subseq path 1) (subseq path-root 1))
	 (from-path-root-backward path path-root)))
      ((string= (subseq path 0 i) (subseq path-root 0 i))
       (from-path-root (subseq path i) (subseq path-root i)))
      (t
       (from-path-root-backward path path-root)))))

(defun path-count-directory-depth (path)
  "Count depth of directories."
  (declare (type string path))
  (loop for ch across path when (char= ch #\/) sum 1))

(defun from-path-root-backward (path path-root)
  "from-path-root, but _only_ backward."
  (declare (type string path))
  (format nil "~{~a~}~a"
	  (make-list (path-count-directory-depth path-root) 
		     :initial-element "../")
	  path))

(defun to-absolute (path)
  "Takes out '../' stuff where possible."
  (declare (type string path))
  (do ((i 0 (+ i 1)))
      ((>= i (length path)) path)
    (declare (type fixnum i))
    (cond
      ((>= (+ i 2) (length path))
       (return path))
      ((and (char= (aref path i) #\.)
	    (string= "../" (subseq path i (+ i 3))))
       (if-let (j (when  (> i 1)
		    (position #\/ path :from-end t :end (- i 1))))
	 (setq path (concatenate 'string 
		      (subseq path 0 (+ j 1)) (subseq path (+ i 3)))
	       i j)
	 (setq path (subseq path (+ i 3))
	       i 0))))))

(defun file-extension (file)
  "Gets file extension."
  (when-let (i (position #\. file :from-end t))
    (values (subseq file i) (subseq file 0 i))))

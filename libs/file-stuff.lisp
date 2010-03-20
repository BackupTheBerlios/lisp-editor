;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :file-stuff
  (:use :common-lisp :generic)
  (:export *file-root* from-file-root to-absolute)
  (:documentation "Some stuff to assist with files and paths."))

(in-package :file-stuff)

(defvar *file-root* nil
  "What is currently 'root' in filesystem.")
(declaim (type string *file-root*))

(defun from-file-root
    (path &optional 
     (file-root (format nil "~a"
		   (or *file-root* *default-pathname-defaults*))))
  "Translates to what the file would be named if . was file-root."
  (declare (type string path file-root))
  (let ((i (position #\/ file-root)))
    (declare (type (or null fixnum) i))
    (cond
      ((or (not i) (>= i (length path)))
       (from-file-root-backward path file-root))
      ((= i 0)
       (if (char= (aref path 0) #\/)
	 (from-file-root (subseq path 1) (subseq file-root 1))
	 (from-file-root-backward path file-root)))
      ((string= (subseq path 0 i) (subseq file-root 0 i))
       (from-file-root (subseq path i) (subseq file-root i)))
      (t
       (from-file-root-backward path file-root)))))

(defun file-count-directory-depth (path)
  "Count depth of directories."
  (declare (type string path))
  (loop for ch across path when (char= ch #\/) sum 1))

(defun from-file-root-backward (path file-root)
  "from-file-root, but _only_ backward."
  (declare (type string path))
  (format nil "~{~a~}~a"
	  (make-list (file-count-directory-depth file-root) 
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
       (if-let j (when  (> i 1)
		   (position #\/ path :from-end t :end (- i 1)))
	 (setq path (concatenate 'string 
		      (subseq path 0 (+ j 1)) (subseq path (+ i 3)))
	       i j)
	 (setq path (subseq path (+ i 3))
	       i 0))))))
		

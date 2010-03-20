;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :file-stuff-test
  (:use :common-lisp :generic :file-stuff))

(in-package :file-stuff-test)

(defun random-path (&key maxdepth (depth (random maxdepth)) maxnum)
  (format nil "~{~a/~}"
     (let (list) (dotimes (n depth list) (push (random maxnum) list)))))

(defun random-path-relative
    (path &key maxrel (rel (random maxrel)) maxdepth maxnum)
  (do ((i (length path) (position #\/ path :from-end t :end i))
       (n rel (- n 1)))
      ((<= n 0) (format nil "~a~a"
			(subseq path 0 n)
			(random-path :maxdepth maxdepth :maxnum maxnum)))))

(defun test
    (&key (cnt 100) (from-depth 7) (maxrel 5) (maxdepth 7) (maxnum 1000))
  (let ((relative (random-path :depth from-depth :maxnum maxnum)))
    (dotimes (n cnt)
      (let*((path
	     (random-path-relative relative
	       :maxrel maxrel :maxdepth maxdepth :maxnum maxnum))
	    (inter
	     (from-file-root path relative))
	    (back-forth
	     (to-absolute (format nil "~a~a" relative inter))))
	(assert (string= back-forth path) nil
		"want ~s = ~s ~s = ~s"
		path relative inter back-forth)))))

(test :cnt 100)

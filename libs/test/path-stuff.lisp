;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(defpackage :path-stuff-test
  (:use :common-lisp :generic :path-stuff))

(in-package :path-stuff-test)

(defvar *maxnum* 1000)
(defvar *maxdepth* 6)

(defun random-path
    (&key (maxdepth *maxdepth*) (depth (random maxdepth)) 
          (maxnum *maxnum*))
  (format nil "~{~a/~}"
     (let (list) (dotimes (n depth list) (push (random maxnum) list)))))

(defun random-path-relative
    (path &key maxrel (rel (random maxrel)))
  (do ((i (length path) (position #\/ path :from-end t :end i))
       (n rel (- n 1)))
      ((<= n 0) (format nil "~a~a" (subseq path 0 n) (random-path)))))

(defun test
    (&key (cnt 100) (from-depth 7) (maxrel 5))
  (dotimes (n cnt)
    (let*((relative (random-path :depth from-depth))
	  (path
	   (random-path-relative relative :maxrel maxrel))
	  (inter
	   (from-path-root path relative))
	  (back-forth
	   (to-absolute (format nil "~a~a" relative inter))))
      (assert (string= back-forth path) nil
	      "want(~a) ~s = ~s ~s = ~s" n
	      path relative inter back-forth))))

;;TODO BUG fails sometimes. suspect random-path-relative
;(test :cnt 100)

(defun test-sub-path-of
    (&key (base-cnt 10) (cnt 10))
  (dotimes (b base-cnt)
    (let*((base (concatenate 'string "root/" (random-path)))
	  (chopped (subseq base 0
				(or (position #\/ base :from-end t) 0))))
      (dotimes (c cnt)
	(let*((random-path (random-path))
	      (add-base    (concatenate 'string base random-path))
	      (add-chopped (concatenate 'string chopped "_" random-path)))
	  (assert (sub-path-of base add-base) nil
		  "Should be sub-path(~a,~a):~%~s~%~s~%~s" b c
		  base add-base (sub-path-of base add-base))
	  (assert (not(sub-path-of chopped add-chopped)) nil
		  "Should _not_ be sub-path(~a,~a):~%~s~%~s~%~s" b c
		  chopped add-chopped (sub-path-of chopped add-chopped)))))))

;(test-sub-path-of :cnt 100)

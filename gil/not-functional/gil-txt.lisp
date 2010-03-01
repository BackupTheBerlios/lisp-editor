;;
;;  Copyright (C) 2010-03-01 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl)

(defpackage gil-txt
  (:use :common-lisp :generic :gil :gil-share)
  (:documentation "Text output of General Interface Library/Language.
Goes by symbol :txt

TODO Totally messed up now.."))

(in-package :gil-txt)

(defun is-whitespace (ch)
  (case ch ((#\Newline #\Space #\Tab) t)))

(defun is-break (ch)
  (not (alpha-char-p ch)))

(defun strip-whitespace (text)
  (do ((i 0 (+ i 1))
       (j 0 j))
      ((>= i (length text))
       (subseq text 0 j))
   ;Write from i to j unless previous was whitespace.
    (unless (and (is-whitespace (aref text i))
		 (or (= i 0)
		     (is-whitespace (aref text (- i 1)))))
      (setf (aref text j) (aref text i))
      (setf- + j 1))))

(defun n-spaces (n)
  (dotimes (k n)
    (write-char #\Space)))

(defun write-tabbed 
    (text &key (line-len *line-len*) 
               (tab-level (* *tab-depth* *tab-step*))
               not-first (txt (strip-whitespace text)))
  (unless (or not-first (> *cur-char-depth* 0))
    (n-spaces tab-level)) ;Add tabs.
  (do ((f 0 f) ;start
       (i *cur-char-depth* (+ i 1)) ;current
       (s 0 (if (is-break (aref txt i)) i s))) ;split
      ((>= i (length txt)) (write-string (subseq txt f i)))
    (when (when (> (- i f) line-len) ;Over newline.
	    (cond
	      ((> (- s f) (* *acceptable-split-ratio* line-len))
	       (write-string (subseq txt f s))
	       (setq f s  i s))
	      (t ;No good cut, make a bad one.
	       (write-string (subseq txt f i))
	       (setq s i  f i))))
      (write-char #\Newline)
      (n-spaces tab-level)))) ;Add tabs.

(defun push-write (obj &key not-first-tab)
  (typecase obj
    (function
     (funcall obj))
    (string
     (write-tabbed obj :not-first not-first-tab))))

(defun push-write-list (objects)
  (mapcar #'push-write objects))

(def-prep (fun function) fun)
(def-prep (thing t) thing)

;;-----------------------List-like------------------------------------------

(def-glist* :series objects
  (push-write-list objects))

(def-glist* :p objects
  (push-write-list objects)
  (wformat "~2%"))

(def-glist* (dot dot-list) list
  (let (not-first)
    (dolist (el list)
      (if not-first
	(n-spaces (* *tab-depth* *tab-step*))
	(setq not-first t))
      (wformat (case (dot-list-style dot)
		 (:disc " * ") (:square " + ") (t (dot-list-style dot))))
      (let ((*tab-depth* (+ *tab-depth* 1)))
	(push-write el :not-first-tab t))
      (wformat "~%"))))

(def-glist (sym symbol) list
  (let ((*tab-depth* (+ *tab-depth* 1)))
    (i-glist *lang* (mk dot-list :style sym) list)))

(defun numbered-list-raw
    (list &key (n 1) (prep "") long-numbers
     (number-hook (lambda (prep n)
		    (format nil "~D~D." prep n))))
  (let ((*tab-depth* (+ *tab-depth* 1)))
    (dolist (th list)
      (wformat "~D ~D" (funcall number-hook prep n)
	               (if long-numbers #\Newline ""))
      (push-write th :not-first-tab t)
      (wformat "~%")
      (setf- + n 1))))

(def-glist* :numbered-list list
  (numbered-list-raw list :long-numbers (> (length list) *long-number*)))

;;--------------------Links; doesn't do actions and notes-------------------

(def-glist* (link gils::follow-link) objects
  (push-write-list objects))

(def-glist* (link link) objects
  (push-write-list objects))

(def-glist* (url url-link) objects
  (push-write-list 
   (cons (prep (format nil "(~a)" (gils::name url))) objects)))

;;-------------------Basic modifiers----------------------------------------

(def-glist* :bold objects
  (wformat "*")
  (push-write-list objects)
  (wformat "*"))
(def-glist :italic objects
  (i-glist *lang* :bold objects))

(def-glist* :underlined objects
  (wformat "_")
  (push-write-list objects)
  (wformat "_"))

;;-------------------Headers------------------------------------------------

(def-glist* (header header) objects
  (let ((level (slot-value header 'gil::level)))
    (wformat (case level
	       (1 "~%--------------------------~%   ")
	       (2 "~%==== ") (3 "---- ") (4 "-+") (t "")))
    (let ((*cur-char-depth* (case level (1 3) (2 5) (3 5) (4 3))))
      (push-write-list objects))
    (wformat (case level
	       (1 "~%--------------------------~%")
	       (2 "====~%") (3 "----~%") (4 "~%") (t "")))))

;Section follow from headers.

;;Image
(def-glist (image base-image) objects
  (declare (ignore objects))
  (format nil "<~D>" (string-downcase (symbol-name (type-of image)))))

(def-glist (image file-image) objects
  (declare (ignore objects))
  (format nil "<~D: ~D>" (string-downcase (symbol-name (type-of image)))
	                 (slot-value image 'gils::file-name)))

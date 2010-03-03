
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

TODO Makes way too much newlines, not tested very deeply."))

(in-package :gil-txt)

(def-call (fun function)
  (funcall fun))

(def-call (null null)
  (declare (ignore null)))

(def-call anything (error "Not recognized: ~a" anything))

(defun remove-gratuous-whitespace (string &key (n 0))
  "TODO this is also "
  (remove-if (lambda (el)
	       (case el ((#\Space #\Tab #\Newline)
			 (setf- + n 1)
			 (when (> n 1) t))
		        (t
			 (setq n 0)
			 nil)))
	     string))

(defvar *have-txt* "")

(defun is-break (ch)
  (not (alpha-char-p ch)))

(defun find-split
    (str &key (split-ratio gils::*acceptable-line-split-ratio*)
              (line-len *line-len*))
  (do ((i (length str) (position-if #'is-break str :from-end t :end i)))
      ((or (null i) (< i (* split-ratio line-len)))
       (if-use i line-len))))

(defvar *indent-delta* 0)

(defun indent ()
  (dotimes (i (- *indent-depth* *indent-delta*))
    (write-char #\Space))
  (setq *indent-delta* 0))

(defun add-txt (str)
  (setq *have-txt* (concatenate 'string *have-txt* str))
  (when (> (length *have-txt*) *line-len*)
    (let ((i (find-split *have-txt*)))
      (indent)
      (write-line (subseq *have-txt* 0 i))
      (setq *have-txt* (subseq *have-txt* i)))))

(def-call (str string)
  (add-txt (substitute #\Space #\Newline 
	    (substitute #\Space #\Tab 
	     (remove-gratuous-whitespace str)))))

(def-call (num number)
  (add-txt (format nil "~a" num)))

(defun dump-txt (&key (with-newline t))
  (indent)
  (if with-newline (write-line *have-txt*) (write-string *have-txt*))
  (setq *have-txt* ""))

;;Lists

(def-glist :series objects
  (call-list objects))

(def-glist :p objects
  (dump-txt)
  (write-line "")
  (call-list objects))

(def-glist (dot dot-list) list
  (dump-txt)
  (let ((*indent-depth* (+ *indent-depth* 2)))
    (dolist (el list)
      (let ((*indent-delta* -2))
	(call (case (slot-value dot 'gils::style)
		(:disc "* ") (:square "+ ") (t ". ")))
        (call el)
	(dump-txt)))))

(def-glist (sym symbol) list
  (let ((*indent-depth* (+ *indent-depth* 1)))
    (i-glist *lang* (mk dot-list :style sym) list)))
#|
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

 (def-glist :numbered-list list
  (numbered-list-raw list :long-numbers (> (length list) *long-number*)))|#

;;Links: generally ignored.

(def-glist (link gils::follow-link) objects
  (declare (ignore link))
  (call-list objects))

(def-glist (link link) objects
  (declare (ignore link))
  (call-list objects))

(def-glist (url url-link) objects ;Refers to the link.
  (call-list objects)
  (call (format nil "(~a)" (gils::name url))))

;;Basic modifiers.

(def-glist :bold objects
  (call "*") (call-list objects) (call "*"))
(def-glist :italic objects
  (call "*") (call-list objects) (call "*"))
(def-glist :underlined objects
  (call "_") (call-list objects) (call "_"))

;;Headers.

(def-glist (header header) objects
  (dump-txt)
  (let ((level (slot-value header 'gils::level)))
    (format t 
     (case level
       (1 "~%--------------------------~%   ")
       (2 "~%==== ") (3 "---- ") (4 "-+") (t "")))
    (call-list objects)
    (dump-txt :with-newline nil)
    (format t
      (case level
	(1 "~%--------------------------~%")
	(2 "====~%") (3 "----~%") (t "")))))

(def-glist (section gils::section) objects
  (with-slots (gils::level gils::title) section
    (call (header gils::level gils::title)))
  (call-list objects)
  (dump-txt))

;Section follow from headers. 
; (TODO perhaps allow those to split into files aswel?)

;;Split
(def-glist (split gils::split) frames
  (error "Text output doesn't do splits"))

;;Images (they just refer to the files.)
(def-glist (image base-image) objects
  (declare (ignore objects))
  (dump-txt)
  (format t "<~D>" (string-downcase (symbol-name (type-of image)))))  

(def-glist (image file-image) objects
  (declare (ignore objects))
  (dump-txt)
  (format t "<~D: ~D>" (string-downcase (symbol-name (type-of image)))
	  (slot-value image 'gils::file-name)))

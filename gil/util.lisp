;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;;

(cl:in-package :cl)

(defpackage gil-util
  (:use :common-lisp :generic :gil :gil-share)
  (:export remove-gratuous-whitespace is-break
	   *indent-delta* indent add-txt dump-txt)
  (:documentation "Some basic utility functions for manipulating strings,\
 writing."))

(in-package :gil-util)

(defun remove-gratuous-whitespace (string &key (n 0))
  "Removes any whitespace in more than pairs."
  (remove-if (lambda (el)
	       (case el ((#\Space #\Tab #\Newline)
			 (setf- + n 1)
			 (when (> n 1) t))
		        (t
			 (setq n 0)
			 nil)))
	     string))

(defun is-break (ch)
  (not (or (alpha-char-p ch) (case ch ((#\. #\, #\: #\;) t)))))

(defun find-split
    (str &key (split-ratio gils::*acceptable-line-split-ratio*)
              (line-len *line-len*))
  "Finds a place to split a line."
  (do ((i (length str) (position-if #'is-break str :from-end t :end i)))
      ((or (null i) (< i (* split-ratio line-len)))
       (if-use i line-len))))

(defun string>-modulo (str-a str-b)
  "String> modulo non-alphanumeric."
  (string> (remove-if-not #'alpha-char-p str-a)
	   (remove-if-not #'alpha-char-p str-b)))

;;Writing utils.
(defvar *have-txt* ""
  "Current text in store.")

(defvar *indent-delta* 0
  "Temporary change in indentation, useful for when you just need a\
 different one for one line.")

(defun indent ()
  "Makes indentation."
  (dotimes (i (- *indent-depth* *indent-delta*))
    (write-char #\Space))
  (setq *indent-delta* 0))

(defun write-txt ()
  (let ((j (find-split *have-txt*)))
    (indent)
    (write-line (subseq *have-txt* 0 j))
    (setq *have-txt* (subseq *have-txt* j))))

(defun add-txt (str)
  "Add text to buffer.(*have-txt*)"
  (setq *have-txt* (concatenate 'string *have-txt* str))
  (when (> (length *have-txt*) *line-len*)
    (write-txt)))

(defun dump-txt (&key (newline t))
  "Empty the current buffer, writing it all."
  (indent)
  (do () ((= (length *have-txt*) 0) (values))
    (write-txt))
  (when newline (write-char #\Newline))
  (setq *have-txt* ""))

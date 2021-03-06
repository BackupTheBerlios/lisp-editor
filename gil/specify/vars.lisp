;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil-vars
  (:use :common-lisp :gil)
  (:export *author*
   *strings-to-package* gil-intern
   *handle-page* *link-page-style*
   *section-page-level* *section-level-modifier*
   *long-number*
   *indent-depth* *indent-step* *line-len* *acceptable-line-split-ratio*
   *attempt-readable* *attempt-shorten*
   *following-directory*)
  (:documentation "Various variables determining how things are done."))

(in-package :gil-vars)

(defvar *author* nil)

(defvar *strings-to-package* :keyword
  "Which package to intern strings into")
(defun gil-intern (sym)
  (typecase sym
    (string (intern sym *strings-to-package*))
    (symbol sym)))

;;Creation of additional documentation at output-time.
(defvar *handle-page* (lambda (name header-result objects)
			(declare (ignore name))
			(glist-list :series
			  (cons header-result objects)))
  "Things that have to be done around a page.")

(defvar *link-page-style* nil "Way the link is\
 followed with regard to page, may try to open new tab or replace old,
 etcetera.. TODO not implemented")

(defvar *section-page-level* 1
  "At what level a page becomes modified.")
(defvar *section-level-modifier* 0
  "Number to add onto level of sections.")

(defvar *long-number* 99
  "When a number is considered long.")

(defvar *following-directory* ""
  "Allows you to tell where the output is supposed to go directory-wise.")

;;Line stuff.
(defvar *indent-depth* 0
  "Current tab depth.")
(defvar *indent-step* 3
  "Step in 'spaces' for indentation.")
(defvar *line-len* 80
  "Maximum line length.")
(defvar *acceptable-line-split-ratio* 0.8
  "If limiting line length, the fraction to split that is still\
 acceptable. TODO more apt name")

;;Telling how to output:
(defvar *attempt-readable* t
  "Whether the output/implementation should try produce human-readable\
 results.")
(defvar *attempt-shorten* t
  "Whether to try make it as short as possible.")

(declaim (type (or string null) *author* *following-directory*)
	 (type (function (t) t) *handle-page*)
	 (type symbol *link-page-style* 
	       *attempt-readable* *attempt-shorten*)
	 (type fixnum *section-page-level* *section-level-modifier*
	       *long-number* *indent-depth* *indent-step* *line-len*)
	 (type single-float *acceptible-line-split-ratio*))

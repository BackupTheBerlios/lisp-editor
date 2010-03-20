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
   *handle-page* *link-page-style*
   *section-page-level* *section-level-modifier*
   *long-number*
   *indent-depth* *indent-step* *line-len* *acceptable-line-split-ratio*
   *page-path* *attempt-readable* *attempt-shorten*
   *cur-page* *cur-pos* *cur-char-depth*)
  (:documentation "Various variables determing how things are done."))

(in-package :gil-vars)

(defvar *author* nil)

;;Creation of additional documentation at output-time.
(defvar *handle-page* #'identity
  "Things that have to be done around a page.")

(defvar *link-page-style* nil "Way the link is\
 followed with regard to page, may try to open new tab or replace old,
 etcetera..")

(defvar *section-page-level* 1
  "At what level a page becomes modified.")
(defvar *section-level-modifier* 0
  "Number to add onto level of sections.")

(defvar *long-number* 99
  "When a number is considered long.")

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
(defvar *page-path* (make-hash-table)
  "Hash table with page names that want an alternate path.\
 (only do it for a reason.)")
(defvar *attempt-readable* t
  "Whether the output/implementation should try produce human-readable\
 results.")
(defvar *attempt-shorten* t
  "Whether to try make it as short as possible.")

;;Internal communication inside packages.(Suggested.)
(defvar *cur-page* ""
  "Suggested variable for communication of current page.")
(defvar *cur-pos* ""
  "Suggested variable of communication of how one would link\
 to the current position.")
(defvar *cur-char-depth* 0
  "Suggested variable of communication of Current character depth.")

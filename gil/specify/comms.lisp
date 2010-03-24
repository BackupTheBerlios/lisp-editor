;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil-comms
  (:use :common-lisp :gil)
  (:export
   *cur-page* *cur-pos* *cur-char-depth* 
   *cur-directory*
   *links* *dead-links* *double-links*
   *contents* *enclosure* *comments-thread* *most-significant-section*)
  (:documentation "Various variables communicating from :info and/or\
 such. Not at the user end."))

(in-package :gil-comms)

;;Internal communication inside packages.(Suggested.)
(defvar *cur-page* ""
  "Suggested variable for communication of current page.")
(defvar *cur-pos* ""
  "Suggested variable of communication of how one would link\
 to the current position.")
(defvar *cur-char-depth* 0
  "Suggested variable of communication of Current character depth.")

(defvar *cur-directory* "")

(defvar *links* (make-hash-table)
  "Keeps track of links.")
(defvar *dead-links* nil
  "Links that failed to have an end to it.")
(defvar *double-links* (make-hash-table)
  "Links that have more than one endpoint.")

(defvar *contents* nil
  "Section info for page of contents is gathered here.")

(defvar *comments-thread* nil
  "Denotes a link to the comments thread if any found.")
(defvar *enclosure* nil)

(defvar *most-significant-section* nil
  "Most important section read so far. This way you can figure out a name\
 for a bit of code by seeing what the most significant section of a part\
 is.")

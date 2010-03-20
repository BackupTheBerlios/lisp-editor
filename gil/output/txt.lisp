;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;;

(cl:in-package :cl)

(defpackage gil-txt
  (:use :common-lisp
	:gil-output-util
	:gil :gil-vars :gil-share :gil-style)
  (:documentation "Text output of General Interface Library/Language.
Goes by symbol :txt

TODO not tested very deeply."))

(in-package :gil-txt)

(def-call anything (error "Not recognized: ~a" anything))

(def-call (str string)
  (add-txt (substitute #\Space #\Newline 
	    (substitute #\Space #\Tab 
	     (remove-gratuous-whitespace str)))))

(def-call (num number)
  (add-txt (format nil "~a" num)))

(def-call (null null) (declare (ignore null)))

(def-call (fun function)
  (funcall fun))

;;Lists

(def-glist :series objects
  (call-list objects))

(def-glist :p objects
  (dump-txt)
  (call-list objects))

(def-glist (dot lister) list
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
    (glist-list (make-instance 'lister :style sym) list)))
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

(def-glist :note objects
  (call "(") (call-list objects) (call ")"))

;;Headers.

(def-glist (header header) objects
  (dump-txt)
  (let ((level (slot-value header 'gils::level)))
    (format t 
     (case level
       (1 "~%--------------------------~%   ")
       (2 "==== ") (3 "---- ") (4 "-+") (t "")))
    (call-list objects) ;TODO one of these makes a newline somehow anyway.
    (dump-txt :newline nil) 
    (format t
      (case level
	(1 "~%--------------------------~%")
	(2 "====~%") (3 "----~%") (t "")))))

(def-glist (section gils::section) objects
  (with-slots (gils::level gils::title) section
    (call (header gils::level gils::title)))
  (call-list objects))

;Section follow from headers. 
; (TODO perhaps allow those to split into files aswel?)

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

;;Table?

(def-glist (table table) objects
  (declare (ignore table objects))
  (error "Tables not(yet?) supported in :txt."))

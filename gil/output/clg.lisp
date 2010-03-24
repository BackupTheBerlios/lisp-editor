;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)
  
(defpackage gil-clg
  (:use :common-lisp :generic :alexandria :denest
	:gil-output-util
	:gil :gil-share :gil-style :gil-vars)
  (:documentation "Output clgs widget.
NOTE try scan it so have documentation.
TODO work in progress. To be in separe asdfsystem, due to clg dependency.")
  (:export ))

(in-package :gil-clg)

;(setq SWANK:*COMMUNICATION-STYLE* :spawn)

(defvar *do-way* (make-hash-table))

(defun do-way (of)
  "Gets/sets settings to do stuff"
  (gethash of do-way))
(defun (setf do-way) (to of)
  "Gets/sets settings to do stuff"
  (setf (gethash of do-way) (if (functionp to) to (constant to))))

(def-call (fun function)
  (let ((swank:*communication-style* :fd-handler))
    (funcall fun)))

(def-call (sym symbol)
  (call (format nil "~a" sym)))

(def-call (string string)
  (make-instance 'gtk:label :str string))

(def-call (num number)
  (call (format nil "~a" num)))

(def-call :newline (call (format nil "~%")))

;(def-call :table-of-contents (call-format "\\tableofcontents"))

(def-call (null null))

(def-call (list list)
  (error "Huh, a list? ~s" list))

;;All referring to pango text attribute markup. ;TODO identical to htmls
;TODO need to pick up *standard-output* something like 'dump' dumping 
; into a list.

(def-xml-surrounding-glist :bold "b")
(def-xml-surrounding-glist :italic "i")
(def-xml-surrounding-glist :underlined "u")
(def-xml-surrounding-glist :strike "s")

(def-xml-surrounding-glist :small "small")
(def-xml-surrounding-glist :big "big")

(def-xml-surrounding-glist :subscript "sub")
(def-xml-surrounding-glist :superscript "sup")

(def-xml-surrounding-glist :code "code")

(def-xml-surrounding-glist :monospace "tt")

;;TODO span elements based on style:
; http://library.gnome.org/devel/pango/stable/PangoMarkupFormat.html

;Ignore what you don't know, because programs are unimaginative.
(def-glist (not-recognized t) objects
  (call-list objects))

(def-glist :p objects
  (case (or (funcall (do-way :p)) :v-pane)
    (:v-pane
     (call-list objects)
     (make-instance 'gtk:v-paned :child (catch-called)))
    (:double-newline
     (call :newline) (call :newline))))

(def-glist :note objects
  (call "(") (call-list objects) (call ")"))

(def-glist :series objects
  (call-list objects))

;;Header and section.
(def-glist (header header) objects
  (case-let (way (or (funcall (do-way :header) header) "big"))
    (:table
     )
    (t ;Just some span/other with newline.
     (xml-surround way
       (call-list objects)
       (call :newline)))))

;TODO work in progress


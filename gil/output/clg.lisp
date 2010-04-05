;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(setq SWANK:*COMMUNICATION-STYLE* :spawn);FD-HANDLER)
(gtk:clg-init-with-threading)
;(gtk:within-main-loop

(cl:in-package :cl-user)
  
(defpackage gil-clg
  (:use :common-lisp :generic :alexandria :denest
	:gil-output-util
	:gil :gil-share :gil-style :gil-vars :gil-comms)
  (:documentation "Output clgs widget.
NOTE try scan it so have documentation.
TODO work in progress. To be in separe asdfsystem, due to clg dependency.")
  (:export ))

(in-package :gil-clg)

(gil::basic-lang :clg)

(defvar *gtk-pages* nil
  "Pages objects collected together.")

(defun make-label (str)
  (make-instance 'gtk:label :label str :selectable t :use-markup t))

(defun final-call (object)
  (denest
   (let*((res (call object)) (str (catch-called))))
   (if (string= str "")
     (typecase res
       (null   (make-label))
       (string (make-label res))
       (list   (make-instance 'gtk:v-box :children res))))
   (let ((label (make-label str))))
   (typecase res
     ((or null string) label)
     (list (make-instance 'gtk:v-box :children `(,@(flatten res) ,label)))
     (t    (make-instance 'gtk:v-box :children (list res label))))))

(plonk-in-window 
 (with-clg-output () (final-call (series (p (b "kitten") " super" 5)
					(p "32354")))))

(defvar *change-page* (lambda (to-page)))

(defmacro with-clg-output (() &body body)
  (with-gensyms (b notebook)
  `(let ((*lang* :clg) (*standard-output* (make-string-output-stream))
	 (*gtk-pages* nil) (,notebook nil)
	 (*change-page* (lambda (to-page)
			  (when ,notebook
			    (setf (gtk:notebook-current-page-num ,notebook)
				  (page-nr to-page))))))
     (let ((,b (progn ,@body)))
       (warn "~s" *gtk-pages*)
       (or (when *gtk-pages*
	     (typecase-let (way (do-way :combine-pages))
	       (null
		(setq notebook (make-instance 'gtk:notebook
				 :children *gtk-pages* :tab-pos :left)))
	       (function
		(funcall way (reverse *gtk-pages*)))))
	   ,b)))))

(defun catch-called () ;Catches pango-formatted stuff.
  (prog1 (get-output-stream-string *standard-output*)
    (setf *standard-output* (make-string-output-stream))))       

(defun plonk-in-window (thing &key (title "Test") (border-width 5) signals)
  (gtk:within-main-loop
    (make-instance 'gtk:window
      :title title :border-width border-width
      :visible t :show-children t
      :child thing
      :signal (append (list 'delete-event (gen:constant nil))
		      signals))))

(plonk-in-window
 (with-clg-output ()
   (final-call (series
		(p (b "boldly") " markupped " 
		   (i "where none has markupped before"))
		(p "Excellent")
		"And stuff" " moar"))))

(let ((*handle-page* (lambda (name header objects)
		       (glist-list :series objects))))
  (plonk-in-window
   (with-clg-output ()
     (call (series
	    (section 1 "sec1" "First section!" "Pretty
 inane! 1st!")
	    (section 1 "sec2" "Second section!" "Pretty inane! 2nd!"))))))

(defun mkwind ()
  (let*((label (make-instance 'gtk:label :label "a" :can-focus t))
	teh-n
	flip
	(button (make-instance 'gtk:button :label "b"
		  :signal `(clicked
			    ,(lambda ()
			     (setf (gtk:notebook-current-page-num teh-n)
				   1)))))
;			     (gtk:widget-hide teh-b)
;			     (gtk:widget-show teh-b)))))
;			     (if flip (gtk:widget-show label)
;			       (gtk:widget-hide label))
;			     (gen:setf- not flip)))))
      (notebook 
       (make-instance 'gtk:notebook :tab-pos :left
	 :children
	 `((,(make-instance 'gtk:label :label "kitten")
	    :tab-label "kitten")
	   (,label :tab-label "a")
	   (,button :tab-label "b")
	   ,(make-instance 'gtk:label :label "cat"))))
	(
  (setq teh-n notebook)
  (plonk-in-window notebook)))

(mkwind)

(defvar *do-way* (make-hash-table))

(defun do-way (of)
  "Gets/sets settings to do stuff"
  (gethash of *do-way*))
(defun (setf do-way) (to of)
  "Gets/sets settings to do stuff"
  (setf (gethash of *do-way*) to))

(def-call (fun function)
  (funcall fun))

(def-call (string string)
  (write-string (substitute #\Space #\Newline
			    (remove-gratuous-whitespace string))))

(def-call (sym symbol)
  (call (format nil "~a" sym)))

(def-call (num number)
  (call (format nil "~a" num)))

(def-call :newline
  (write-char #\Newline))

;(def-call :table-of-contents (call-format "\\tableofcontents"))

(def-call (null null))

(def-call (list list)
  (error "Huh, a list? ~s" list))

(def-call (other t)
  other)

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

(def-glist :series objects
  (flatten (remove-if (lambda (el) (typecase el ((or string null) t)))
		      (call-list objects))))

;Ignore what you don't know, because programs are unimaginative.
(def-glist (ignore t) objects
  (declare (ignore ignore))
  (call (glist-list :series objects)))

(defun dump () 
  (make-instance 'gtk:label :label (catch-called)
		 :selectable t :use-markup t))

(def-glist :p objects
  (case (or (do-way :p) :v-pane)
    (:v-pane
      (list 
       (prog1 (dump)
	 (call-list objects))
       (make-instance 'gtk:label :label (catch-called)
		      :selectable t :use-markup t)))
    (:double-newline
     (call :newline) (call :newline))))

(def-glist :note objects
  (call "(") (call-list objects) (call ")"))

;TODO align with text.
;TODO figure out if position can be done more accurately.
(def-glist (link follow-link) objects
  (list (prog1 (dump)
	  (call-list objects))
	(make-instance 'button :label (catch-called)
	  :selectable t :use-markup t
	  :signal `(clicked
		    ,(rcurry *change-page* (gil-info::link-page link))))))

;;Header and section.
(def-glist (header header) objects
  (flet ((call-it ()
	   (call-list objects) (call :newline)))
    (case-let (way (or (when-let (way (do-way :header))
			 (funcall way header))
		       (case (slot-value header 'gils::level)
			 ((1 2) :bold-big) (3 "big") (t "b"))))
      (:bold-big (xml-surround "b" (xml-surround "big" (call-it))))
    ;TODO other ways..
      (string    (xml-surround way (call-it)))
      (function  (funcall way objects)) ;Really taking control!
      (t         (error "Didn't recognize way ~s" way)))))

(def-glist (section section) objects
  (denest
   (with-slots (gils::level gils::name gils::title) section)
   (let ((header (link-pos gils::name (header gils::level gils::title)))))
   (cond
     ((> gils::level *section-page-level*)
      (final-call
       (glist-list :series
	 (cons header objects)))))
   (t)
   (let*((*cur-page* (get-page gils::name))))
   (typecase-let (way (when-let (way (do-way :new-page))
			(funcall way section)))
     (null ;Default behavior just pushes it on the list.
      (push (list (final-call
		   (funcall *handle-page* gils::name header objects))
		  :tab-label (let ((*lang* :txt))
			       (substitute #\Space #\Newline
				 (with-output-to-string (*standard-output*)
				   (call gils::title)
				   (dump-txt :newline nil)))))
	    *gtk-pages*)
      (call (has-own-page gils::name)))
     (function
      (funcall way objects))
     (t
      (error "Didn't recognize way ~s" way)))))
	
;TODO work in progress

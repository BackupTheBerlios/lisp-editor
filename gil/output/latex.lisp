;;
;;  Copyright (C) 2010-03-01 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl)

(defpackage gil-latex
  (:use :common-lisp :denest
	:gil-output-util
	:gil :gil-vars :gil-share :gil-style)
  (:documentation "Latex output of General Interface Library/Language.
Goes by symbol :latex

TODO no styles, latex aught to be able to do a bunch of it.

TODO/NOTE completely untested other then inspection of result.

TODO equations would be neat to have, and crazy not to have.
1)I want the capability to have identical interface in other outputs!
2)Via s-expressions first, non-s-expression input form later."))

(in-package :gil-latex)

(gil::basic-lang :latex)

;;Surround, same idea as for html, except here styles don't tag along.
(defmacro surround (with &body body)
  `(surround-fn ,with (lambda () ,@body)))

(defun surround-fn (with fill)
  "Puts \\name{} around."
  (when *attempt-readable* (dump-txt :newline nil))
  (format t "\\~a{" with)
  (funcall fill)
  (when *attempt-readable* (dump-txt :newline nil))
  (format t "}"))

(defmacro surround-be (with &body body)
  `(surround-fn ,with (lambda () ,@body)))

(defmacro call-format (cstr &rest args)
  `(call (format nil ,cstr ,@args)))

(defun surround-be-fn (with fill)
  "Puts \\begin{} and \\end{} around."
  (when *attempt-readable* (dump-txt))
  (call-format "\\begin{~a}~%" with)
  (funcall fill)
  (when *attempt-readable* (dump-txt))
  (call-format "\\end{~a}~%" with))

(def-call (sym symbol)
  (call-format "\\~a" sym))

(def-call (string string)
  (if *attempt-readable* (add-txt string) (format t string)))

(def-call (num number)
  (call-format "~a" num))

(def-call :newline (call-format "~%"))

;TODO perhaps mode for turning this into gil-generated table of contents.
(def-call :table-of-contents (call-format "\\tableofcontents"))

(def-call (null null))

(def-call (list list)
  (error "Huh, a list? ~s" list))

;Ignore what you don't know, because programs are unimaginative.
(def-glist (not-recognized t) objects
  (call-list objects))

;;What is the difference between Latex quote and quotation??
(def-glist :quotation objects
  (surround-be "quote" (call-list objects)))

(def-glist :p objects ;;TODO catch styles.
  (surround-be "flush-left" (call-list objects)))

;;Currently no special call. Could use gil-utils writing functions and
;; make it neat. (It should keep off the latex stuff, though.

(def-glist (dot lister) list
  (surround-be "itemize"
    (dolist (el list) ;TODO more of them.
      (call-format "~%\\item~D " (case (slot-value dot 'gils::style)
			       (:square "[+]") (t "")))
      (call el))))

(def-glist :numbered-list list
  (surround-be "enumerate" ;TODO more of them.
    (dolist (el list)
      (call-format "~%\\item ")
      (call el))))

(def-glist :descriptions list
  (surround-be "description"
    (dolist (el list)
      (call "\\item [") (call (car el)) (call "]")
      (call-list (cdr el)))))

;Basic modifiers.
(def-glist :bold objects
  (surround "textbf" (call-list objects)))

(def-glist :italic objects
  (surround "textit" (call-list objects)))

(def-glist :underlined objects
  (surround "underline" (call-list objects)))

;;TODO might be able to do links.

;;Headers an section.
(defun do-header (header title)
  (with-slots (gils::level) header
    (surround (format nil "~asection" (case gils::level
					 ((1 2) "") (3     "sub")
					 (t     "subsub")))
      (call title))))

(def-glist (header header) objects
  (do-header header (glist-list :series objects)))

(def-glist (section section) objects
  (do-header section (slot-value section 'gils::title))
  (let ((*indent-depth* 2))
    (call-list objects)))

;;Image
(def-glist (image base-image) objects
  (declare (ignore objects))
  (call-format "<~D>" (string-downcase (symbol-name (type-of image)))))

(def-glist (image file-image) objects ;TODO pretty basic.
  (declare (ignore objects))
  (call-format "\includegraphics{~D}" (slot-value image 'gils::file-name)))

;;Table

(defun last-ch (str)
  (aref str (- (length str) 1)))

(defun table-guts (table cols elements)
  (denest:denest
   (let ((*in-table* t)))
   (with-slots (gils::width gils::border) table)
   (surround-be (if gils::width "tabular*" "tabular")
     (if gils::width
       (case (last-ch gils::width)
	 (#\% (call-format "{0.~a\\textwidth}{@{\\extracolsep{\\fill}}" 
		       (subseq gils::width 0 (- (length gils::width) 2))))
	 (t   (call-format "{~a}{@{\\extracolsep{\\fill}}" gils::width)))
       (call-format "{")) ;TODO Hopefully it will figure it out itself?
    ;Do it with multicolumn. (Inefficient, but ah well.)
     (when gils::border (call-format "|"))
     (funcall cols)
     (when gils::border (call-format "|}"))
     (when gils::border (call-format "\\hline"))
     (funcall elements)
     (when gils::border (call-format "\\hline")))))

(defun x-align-style  (x-align)
  (case x-align
    ((:low :left) "l")
    ((:hight :right) "r")
    ((:center :justify) "c")
    (t "c")))

(def-glist (table col-table) list
  (table-guts table
    (lambda ()
      (call-format "~{~a~}" 
        (mapcar (lambda (col)
		  (let ((x-align (x-align-style
				  (or (getf col :align)
				      (getf col :x-align)))))
		    (x-align-style x-align)))
		(slot-value table 'gils::cols))))
    (lambda ()
      (dolist (el list)
	(dolist (s-el el)
	  (typecase s-el
	    (table-el (call s-el)) ;Might override it with multicolumn.
	    (t        (call s-el) (call-format "&")))
	  (when *attempt-readable* (dump-txt :newline nil))
	  (call-format "\\\\~%"))))))

(def-glist (table table) list
  (table-guts table
    (lambda ()
      ;All done with multicolumn for table.
      (call-format "~{~a~}"
	       (make-list (reduce #'max (mapcar #'length list))
			  :initial-element #\X)))
    (lambda ()
      (dolist (el list)
	(dolist (s-el el)
	  (typecase s-el
	    (table-el (call s-el))
	    (t        (call (table-el nil s-el))))
	  (when *attempt-readable* (dump-txt :newline nil))
	  (call-format "\\\\~%"))))))

(def-call (el table-el)
  (assert *in-table* nil "Table elements _must_ be in a table!")
  (with-slots (gils::contents gils::y-span gils::x-align) el
    (surround "multicolumn"
      (call-format "{~a}{~a}"
	       (or gils::y-span 1) (x-align-style gils::x-align))
      (call-list gils::contents)))
  (call-format "&"))

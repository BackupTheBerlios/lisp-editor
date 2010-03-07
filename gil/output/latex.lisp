;;
;;  Copyright (C) 2010-03-01 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl)

(defpackage gil-latex
  (:use :common-lisp :generic :gil :denest :gil-share :gil-util)
  (:documentation "Latex output of General Interface Library/Language.
Goes by symbol :latex

TODO changes were made to break it, in process of making it work again.

TODO no styles, latex aught to be able to do a bunch of it.

TODO/NOTE completely untested other then inspection of result.
TODO equations would be neat to have, and crazy not to have.
1)I want the capability to have identical interface in other outputs!
2)S-expressions first, non-s-expression input form later."))

(in-package :gil-latex)

;;Surround, same idea as for html, except here styles don't tag along.
(defmacro surround (with &body body)
  `(surround-fn ,with (lambda () ,@body)))

(defun surround-fn (with fill)
  "Puts \\name{} around."
  (format t "\\~a{" with)
  (funcall fill)
  (format t "}"))

(defmacro surround-be (with &body body)
  `(surround-fn ,with (lambda () ,@body)))

(defun surround-be-fn (with fill)
  "Puts \\begin{} and \\end{} around."
  (format t "\\begin{~a}~%" with)
  (funcall fill)
  (format t "\\end{~a}~%" with))

(def-call (fun function)
  (funcall fun))

(def-call (sym symbol)
  (format t "\\~a" sym))

(def-call (string string)
  (format t string))

(def-call (num number)
  (format t "~a" num))

(def-call :table-of-contents (format t "\\tableofcontents"))

;;What is the difference between Latex quote and quotation??
(def-glist :quotation objects
  (surround-be "quote" (call-list objects)))

(def-glist :p objects ;;TODO catch styles.
  (surround-be "flush-left" (call-list objects)))

;;Currently no special call. Could use gil-utils writing functions and
;; make it neat. (It should keep off the latex stuff, though.

(def-glist (dot dot-list) list
  (surround-be "itemize"
    (dolist (el list) ;TODO more of them.
      (format t "~%\\item~D " (case (slot-value dot 'gils::style)
			       (:square "[+]") (t "")))
      (call el))))

(def-glist :numbered-list list
  (surround-be "enumerate" ;TODO more of them.
    (dolist (el list)
      (format t "~%\\item ")
      (call el))))

(def-glist :descriptions list
  (surround-be "description"
    (dolist (el list)
      (call "\\item [") (call (car el)) (call "]")
      (call-list (cdr el)))))

;;TODO might be able to do links.

(def-glist :bold objects
  (surround "textbf" (call-list objects)))

(def-glist :italic objects
  (surround "textit" (call-list objects)))

(def-glist :underlined objects
  (surround "underline" (call-list objects)))

(defun do-header (header title)
  (with-slots (gils::level) header
    (surround (format nil "~Dsection" (case gils::level
					 ((1 2) "") (3     "sub")
					 (t     "subsub")))
      (call title))))

(def-glist (header header) objects
  (do-header header objects))

(def-glist (section section) objects
  (do-header section (slot-value section 'gils::title))
  (let ((*indent-depth* 2))
    (call-list objects)))

;;Image
(def-glist (image base-image) objects
  (declare (ignore objects))
  (format t "<~D>" (string-downcase (symbol-name (type-of image)))))

(def-glist (image file-image) objects ;TODO pretty basic.
  (declare (ignore objects))
  (format t "\includegraphics{~D}" (slot-value image 'gils::file-name)))

;;Table

(defun last-ch (str)
  (aref str (- (length str) 1)))

(defun table-guts (table cols elements)
  (denest:denest
   (let ((gils::*in-table* t)))
   (with-slots (gils::width gils::border) table)
   (surround-be (if gils::width "tabular*" "tabular")
     (if gils::width
       (case (last-ch gils::width)
	 (#\% (format t "{0.~a\\textwidth}{@{\\extracolsep{\\fill}}" 
		       (subseq gils::width 0 (- (length gils::width) 2))))
	 (t   (format t "{~a}{@{\\extracolsep{\\fill}}" gils::width)))
       (format t "{")) ;TODO Hopefully it will figure it out itself?
    ;Do it with multicolumn. (Inefficient, but ah well.)
     (when gils::border (format t "|"))
     (funcall cols)
     (when gils::border (format t "|}"))
     (when gils::border (format t "\\hline"))
     (funcall elements)
     (when gils::border (format t "\\hline")))))

(defun x-align-style  (x-align)
  (case x-align
    ((:low :left) "l")
    ((:hight :right) "r")
    ((:center :justify) "c")
    (t "c")))

(def-glist (table col-table) list
  (table-guts table
    (lambda ()
      (format t "~{~a~}" 
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
	    (t        (call s-el) (format t "&")))
	  (format t "\\\\~%"))))))

(def-glist (table table) list
  (table-guts table
    (lambda ()
      ;All done with multicolumn for table.
      (format t "~{~a~}"
	       (make-list (reduce #'max (mapcar #'length list))
			  :initial-element #\X)))
    (lambda ()
      (dolist (el list)
	(dolist (s-el el)
	  (typecase s-el
	    (table-el (call s-el))
	    (t        (call (table-el nil s-el))))
	  (format t "\\\\~%"))))))

(def-call (el table-el)
  (assert gils::*in-table* nil "Table elements _must_ be in a table!")
  (with-slots (gils::contents gils::y-span gils::x-align) el
    (surround "multicolumn"
      (format t "{~a}{~a}"
	       (or gils::y-span 1) (x-align-style gils::x-align))
      (call-list gils::contents)))
  (format t "&"))




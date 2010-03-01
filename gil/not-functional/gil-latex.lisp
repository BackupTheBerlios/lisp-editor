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
  (:use :common-lisp :generic :gil :gil-share)
  (:documentation "Latex output of General Interface Library/Language.
Goes by symbol :latex
TODO/NOTE completely untested other then inspection of result.
TODO equations would be neat to have, and crazy not to have.
1)I want the capability to have identical interface in other outputs!
2)S-expressions first, non-s-expression input form later."))

(in-package :gil-latex)

(make-gil-definer :latex def-gil-method def-gil-method*)

(def-gil-method* i-prep (str string) ()
  (wformat str))

(def-gil-method i-glist (sep t) ((list list))
  (i-glist :txt sep list))

(def-gil-method* i-glist (dot dot-list) ((list list))
  (wformat "\\begin{itemize}")
  (dolist (el list)
    (wformat "~%\\item~D " (case (dot-list-style dot)
			     (:square "[+]") (t "")))
    (call el))
  (wformat "~%\\end{itemize}~%"))

(def-gil-method* i-glist :numbered-list ((list list))
  (wformat "\\begin{enumerate}")
  (dolist (el list)
    (wformat "~%\\item ")
    (call el))
  (wformat "~%\\end{enumerate}~%"))    

(def-gil-method i-action (action t) (object)
  (i-action :txt action object))

;TODO i am sure it can do it.
(def-gil-method i-note (link link) (object)
  (i-note :txt link object))

(def-gil-method* i-note :bold (object)
  (wformat "\\textbf{") (call object) (wformat "}"))

(def-gil-method* i-note :italic (object)
  (wformat "\\textit{") (call object) (wformat "}"))

;(def-gil-method i-note :underlined (object)
;  )

(def-gil-method* i-header (n integer) (object)
  (wformat "~%\\~Dsection{" (case n
			   ((1 2) "")
			   (3     "sub")
			   (t     "subsub")))
  (call object)
  (wformat "}~%"))

(defmethod i-section ((lang (eql :html))
		      level name (object string) (paragraphs list))
  (i-section :html level name (lambda () (wformat object)) paragraphs))

;;Image
(def-gil-method i-prep (image base-image) ()
  (format nil "<~D>" (string-downcase (symbol-name (type-of image)))))

(def-gil-method i-prep (image file-image) () ;TODO pretty basic.
  (format nil "\includegraphics{~D}" (file-name image)))

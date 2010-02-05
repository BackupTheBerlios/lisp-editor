
(cl:in-package :cl)

(defpackage gil-txt
  (:use :common-lisp :generic :gil :gil-share)
  (:documentation "Text output of General Interface Library/Language.
Goes by symbol :txt

TODO slightly excessive newlining."))

(in-package :gil-txt)

(make-gil-definer :txt def-gil-method def-gil-method*)

(defun is-whitespace (ch)
  (case ch ((#\Newline #\Space #\Tab) t)))

(defun is-break (ch)
  (not (alpha-char-p ch)))

(defun strip-whitespace (text)
  (do ((i 0 (+ i 1))
       (j 0 j))
      ((>= i (length text))
       (subseq text 0 j))
   ;Write from i to j unless previous was whitespace.
    (unless (and (is-whitespace (aref text i))
		 (or (= i 0)
		     (is-whitespace (aref text (- i 1)))))
      (setf (aref text j) (aref text i))
      (setf- + j 1))))

(defun n-spaces (n)
  (dotimes (k n)
    (write-char #\Space)))

(defun write-tabbed 
    (text &key (line-len *line-len*) 
               (tab-level (* *tab-depth* *tab-step*))
               not-first (txt (strip-whitespace text)))
  (unless (or not-first (> *cur-char-depth* 0))
    (n-spaces tab-level)) ;Add tabs.
  (do ((f 0 f) ;start
       (i *cur-char-depth* (+ i 1)) ;current
       (s 0 (if (is-break (aref txt i)) i s))) ;split
      ((>= i (length txt)) (write-string (subseq txt f i)))
    (when (when (> (- i f) line-len) ;Over newline.
	    (cond
	      ((> (- s f) (* *acceptable-split-ratio* line-len))
	       (write-string (subseq txt f s))
	       (setq f s  i s))
	      (t ;No good cut, make a bad one.
	       (write-string (subseq txt f i))
	       (setq s i  f i))))
      (write-char #\Newline)
      (n-spaces tab-level)))) ;Add tabs.

(defun push-write (obj &key not-first-tab)
  (typecase obj
    (function
     (funcall obj))
    (string
     (write-tabbed obj :not-first not-first-tab))))

(def-gil-method i-prep (string string) ()
  string)

;;-----------------------List-like------------------------------------------

(def-gil-method* i-glist :series ((list list))
  (mapcar #'push-write list))

(def-gil-method* i-glist :p ((list list))
  (mapcar #'push-write list)
  (wformat "~2%"))

(def-gil-method* i-glist (dot dot-list) ((list list))
  (let (not-first)
    (dolist (el list)
      (if not-first
	(n-spaces (* *tab-depth* *tab-step*))
	(setq not-first t))
      (wformat (case (dot-list-style dot)
		 (:disc " * ") (:square " + ") (t (dot-list-style dot))))
      (let ((*tab-depth* (+ *tab-depth* 1)))
	(push-write el :not-first-tab t))
      (wformat "~%"))))

(def-gil-method i-glist :list ((list list))
  (let ((*tab-depth* (+ *tab-depth* 1)))
    (i-glist *lang* (mk dot-list :style :disc) list)))

(def-gil-method i-glist :alt-list ((list list))
  (let ((*tab-depth* (+ *tab-depth* 1)))
    (i-glist *lang* (mk dot-list :style :square) list)))

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

(def-gil-method* i-glist :numbered-list ((list list))
  (numbered-list-raw list :long-numbers (> (length list) *long-number*)))

;;--------------------Links; doesn't do actions and notes-------------------

(def-gil-method* i-action (link t) ((str string))
  (wformat str))

(def-gil-method* i-action (link link) (object)
  object)

(def-gil-method* i-note (link link) (object)
  object)

;;-------------------Basic modifiers----------------------------------------

(def-gil-method* i-note :bold (object)
  (wformat "*")
  (call object)
  (wformat "*"))
(def-gil-method* i-note :italic (object)
  (wformat "*")
  (call object)
  (wformat "*"))

(def-gil-method* i-note :underlined (object)
  (wformat "_")
  (call object)
  (wformat "_"))

;;-------------------Comments----------------------------------------------
(def-gil-method* i-note :comment (object))

;;-------------------Headers------------------------------------------------

(def-gil-method* i-header 1 (object)
  (wformat "~%--------------------------~%   ")
  (let ((*cur-char-depth* 3))
    (call object))
  (wformat "~%--------------------------~%"))

(def-gil-method* i-header 2 (object)
  (wformat "~%==== ")
  (let ((*cur-char-depth* 5))
    (call object))
  (wformat " ====~%"))

(def-gil-method* i-header 3 (object)
  (wformat "---- ")
  (let ((*cur-char-depth* 5))
    (call object))
  (wformat " ----~%"))

(def-gil-method* i-header 4 (object)
  (wformat "-+ ")
  (let ((*cur-char-depth* 3))
    (call object))
  (wformat "~%"))

;Sections follow from headers.
(defmethod i-section ((lang (eql :txt))
		      level name (object string) (paragraphs list))
  (i-section *lang* level name (lambda () (wformat object)) paragraphs))

;;Image
(def-gil-method i-prep (image base-image) ()
  (format nil "<~D>" (string-downcase (symbol-name (type-of image)))))

(def-gil-method i-prep (image file-image) ()
  (format nil "<~D: ~D>" (string-downcase (symbol-name (type-of image)))
	                 (file-name image)))

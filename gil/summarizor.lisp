
(cl:in-package :cl-user)

(defpackage gil-contents
  (:use :common-lisp :generic :denest :gil :gil-share :gil-read))

(in-package :gil-contents)

(defvar *contents* nil)

(defun gather-contents (&rest files)
  (let ((*contents* nil))
    (mapcar (curry #'gil-read::gil-execute :lang :contents)
	    files)
    (reverse *contents*)))

(defun rank (list &key (compare #'<=))
  (when (null list)
    (return-from rank nil))
  (gen:if-let i (position-if (gen:curry compare (car list)) (cdr list))
    (cons (cons (car list)
		(rank (subseq (cdr list) 0 i) :compare compare))
	  (rank (subseq (cdr list) i) :compare compare))
    (list (cons (car list) (rank (cdr list) :compare compare)))))

(defun organize-contents (contents)
  "Organizes contents, producing a lit of the elements based on level."
  (rank contents :compare (lambda (a b)
			    (< (car a) (car b)))))
#|
 (defun use-contents
    (contents &key number
     (split-per-level 0.1) (split-per-level-way :relative))
  (let ((contents (organize-contents contents)))
    (apply #'glist
	   (cons :series
		 (mapcar (lambda (el)
			   (glist mk-split |#
		      
(make-gil-definer :contents def-gil-method def-gil-method*)

(def-gil-method i-prep (thing t) () thing)
(def-gil-method i-glist (thing t) (list) nil)
(def-gil-method i-note (thing t) (object) nil)
(def-gil-method i-action (action t) (object) nil)

(def-gil-method i-section (level t) (name object list)
  (print name)
  (push (list level name object (car list)) *contents*))



;;TODO am thinking.. perhaps this is dumb.. 
;; Just take the information as code, go round the problem of objects.

(cl:in-package :cl)

(defpackage gil-html
  (:use :common-lisp :generic :gil :gil-share)
  (:documentation "Produces .gil files, pretty inane, but a way to record\
 results. TODO (eh, how to do those objects..)"))

(in-package :gil-html)

(make-gil-definer :gil def-gil-method def-gil-method*)

(def-gil-method i-prep thing ()
  thing)

;;glist
(def-gil-method* i-glist (way symbol) ((things list))
  (format t "(glist ~s " way)
  (mapcar #'funcall things)
  (format t ")"))

(def-gil-method* i-glist :p ((things list))
  (format t "(p ") (mapcar #'funcall things) (format t ")"))
(def-gil-method* i-glist :list ((things list))
  (format t "(point-list ") (mapcar #'funcall things) (format t ")"))
(def-gil-method* i-glist :alt-list ((things list))
  (format t "(alt-point-list ") (mapcar #'funcall things) (format t ")"))

;;note
(def-gil-method i-note :bold (obj)
  (format t "(b ") (funcall obj) (format t ")"))
(def-gil-method i-note :italic (obj)
  (format t "(i ") (funcall obj) (format t ")"))
(def-gil-method i-note :underlined (obj)
  (format t "(u ") (funcall obj) (format t ")"))

(def-gil-method* i-note (link link) (object)
  (format t "(link-pos ")
  (funcall object)
  (with-slots (name) link
    (format t " ~s)" name)))

;;action
(def-gil-method* i-action (link link) (object)
  (format t "(link ")
  (funcall object)
  (with-slots (name page) link
    (format t " ~s ~s)" name page)))

;;Headers &Sections

(def-gil-method* i-header (level integer) object
  (format t "(header ~D " level)
  (funcall object) (format t ")"))

(def-gil-method* i-section (level integer) (name object (paragraphs list))
  (format t "(section ~D ~D " name level)
  (funcall object) (format t " ")
  (mapcar #'funcall paragraphs))

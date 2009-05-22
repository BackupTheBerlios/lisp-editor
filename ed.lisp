
(defpackage #:lisp-ed
  (:use #:common-lisp #:generic #:ltk)
  (:export start))

(in-package #:lisp-ed)

(defun start (&key (layout #'default-layout))
  (with-ltk ()
    (funcall layout)))

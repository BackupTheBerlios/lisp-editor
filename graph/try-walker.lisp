
(in-package :cl-walker)

(walk-form '(defun sqr (x)
	     (* x x)))


(wl

(documentation #'map-ast t)

(require :documentation-template)


(documentation-template:create-template :cl-walker :target "/home/jasper/oproj/cl-walker"
					:maybe-skip-methods-p t)
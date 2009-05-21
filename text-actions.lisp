
(in-package #:lisp-ed)

;;Basic copy and paste.
(def-text-fun cut
  (format-wish "tk_textCut ~a" (widget-path txt)))

(def-text-fun copy
  (format-wish "tk_textCopy ~a" (widget-path txt)))

(def-text-fun paste
  (format-wish "tk_textPaste ~a" (widget-path txt)))

;;Emacs-like selection. ;;TODO

;;Emacs-like copy and paste.

(defparameter *text-copies-max-cnt* 16)
(defparameter *text-copies-cnt* 0)
(defparameter *text-copies* nil)

(defun e-add-copy (copy)
  (setf- + *text-copies-cnt* 1)
  (when (> *text-copies-cnt* *text-copies-max-cnt*)
    (setf- subseq *text-copies* 0 *text-copies-max-cnt*))
  (push copy *text-copies))

(defun e-cut-part (text cut)
  (let*((from (get-cursor-pos text))
	(to   (forward-from-position from (length cut))))
    (e-add-copy cut)
    (delete-text text from to)))

(def-text-fun e-delete-word-rest
  "Emacs default M-d."
  (e-cut-part text (get-current-word-after-cursor text)))

(def-text-fun e-delete-word-rest
  "Emacs default M-backspace."
  (e-cut-part text (get-current-word-before-cursor text)))

(def-text-fun e-kill
  "Emacs-like kill. (as defaultly bound to M-k)"
  (e-cut-part text (get-current-line-after-cursor text)))

(defun e-cycle ()
  "Cycles the selection of copies. Emacs M-y"
  (setf (cdr (last *text-copies*)) (pop *text-copies*)))

(def-text-fun e-paste
  "Pastes the top copy. Enacs C-y"
  (insert-text text (car *text-copies*)))



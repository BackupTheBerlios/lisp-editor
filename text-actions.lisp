
(in-package #:lisp-ed)

;;Basic copy and paste.
(def-text-fun cut
  (format-wish "tk_textCut ~a" (widget-path text)))

(def-text-fun copy
  (format-wish "tk_textCopy ~a" (widget-path text)))

(def-text-fun paste
  (format-wish "tk_textPaste ~a" (widget-path text)))

;;Emacs-like selection. ;;TODO

;;Emacs-like copy and paste.

(defparameter *text-copies-max-cnt* 16)
(defparameter *text-copies* nil)

(defun e-add-copy (copy)
  (push copy *text-copies*)
  (when (> (length *text-copies*) *text-copies-max-cnt*)
    (setf- subseq *text-copies* 0 *text-copies-max-cnt*)))

(defun e-cut-part (text cut &optional neg)
  (let*((from (get-cursor-pos text))
	(to   (forward-from-position from (length cut))))
    (e-add-copy cut)
    (if neg
      (delete-text text to from)
      (delete-text text from to))))

(def-text-fun e-delete-word-after
  "Emacs default M-d."
  (e-cut-part text (get-current-word-after-cursor text)))

(def-text-fun e-delete-word-before
  "Emacs default M-backspace."
  (e-cut-part text (get-current-word-before-cursor text) t))

(def-text-fun e-kill
  "Emacs-like kill. (as defaultly bound to M-k)"
  (e-cut-part text (get-current-line-after-cursor text)))

(defun e-cycle ()
  "Cycles the selection of copies. Emacs M-y"
  (unless (null (cdr *text-copies*))
    (setf (cdr (last *text-copies*)) (pop *text-copies*))))

(def-text-fun e-paste
  "Pastes the top copy. Enacs C-y"
  (print :ep)
  (unless (null *text-copies*)
    (insert-text text (car *text-copies*))))



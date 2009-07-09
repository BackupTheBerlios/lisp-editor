
(in-package #:lisp-ed)

;;Basic copy and paste.
(defun cut (text)
  (format-wish "tk_textCut ~a" (widget-path text)))

(defun copy (text)
  (format-wish "tk_textCopy ~a" (widget-path text)))

(defun paste (text)
  (format-wish "tk_textPaste ~a" (widget-path text)))

;;Emacs-like selection. ;;TODO

;;Emacs-like copy and paste.

(defparameter *text-copies-max-cnt* 16)
(defparameter *text-copies* nil)

(defparameter *concatenate-sequential-copies* nil
  "Whether to concatenatesequential copies. Current method not good \
enough. (Hopefully C-Y, paste-and cycle helps.)")
(defparameter *previous-copy-dir* t)
(defparameter *previous-copy-count* -2)

(defun e-add-copy (copy)
  (when (= (length copy) 0)
    (return-from e-add-copy))
  (if (and (= (- *event-counter* 1) *previous-copy-count*)
	   *concatenate-sequential-copies*)
    (setf (car *text-copies*) ;Previous was of same, lengthen current.
	  (if *previous-copy-dir* ;Keep the direction correct.
	    (concatenate 'string copy (car *text-copies*))
	    (concatenate 'string (car *text-copies*) copy)))
    (push copy *text-copies*))
  (setf *previous-copy-count* *event-counter*)
  (when (> (length *text-copies*) *text-copies-max-cnt*)
    (setf- subseq *text-copies* 0 *text-copies-max-cnt*)))

(defun e-cut-part (text cut &optional neg)
  (let*((from (get-cursor-pos text))
	(to   (forward-from-position from (length cut))))
    (e-add-copy cut)
    (setf *previous-copy-dir* neg)
    (if neg
      (delete-text text to from)
      (delete-text text from to))))

(defun e-delete-word-after (text)
  "Emacs default M-d."
  (e-cut-part text (get-current-word-after-cursor text)))

(defun e-delete-word-before (text)
  "Emacs default M-backspace."
  (e-cut-part text (get-current-word-before-cursor text) t))

(defun e-kill (text)
  "Emacs-like kill. Emacs default M-k"
  (e-cut-part text (get-current-line-after-cursor text)))

(defun e-cycle ()
  "Cycles the selection of copies. Emacs default M-y"
  (unless (null (cdr *text-copies*))
    (setf (cdr (last *text-copies*)) (list (pop *text-copies*)))))

(defun e-paste (text)
  "Pastes the top copy. Emacs default C-y"
  (unless (null *text-copies*)
    (when (= (length (car *text-copies*)) 0)
      (error "How did a zero-length part get copied?"))
    (insert-text text (car *text-copies*))))

(defun e-paste-and-cycle (text)
  (e-paste text)
  (e-cycle))


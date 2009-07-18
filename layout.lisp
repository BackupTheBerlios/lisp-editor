
(in-package #:lisp-ed)

(defun in-sequence (&rest funs)
  (lambda (&rest args)
    (dolist (f funs)
      (apply f args))))

(defun default-buffer-bindings (text)
  (nub-copy-text-bindings text)
  (emacs-like-copy-text-bindings text)
  (emacs-like-cursor-motion text)
  (event-mode-bindings text)
  (selection-bindings text)
  (file-operations text)
  (evaluate-binding text))

(defun default-layout (&key (buffer-preset #'default-buffer-bindings))
  (let ((frame (make-instance 'frame :takefocus t)))
    (setf *buffers* nil) ;;TODO scrollbar.
    
    (add-buffer :buffer-preset buffer-preset :master frame)
    (add-buffer :buffer-preset buffer-preset :master frame)
    (setf *focus* *buffers*)
    (pack frame :side :top :fill :both :expand t)
    (pack (setf *entry* (make-default-entry)) :side :bottom
	  :fill :x)))

(print *event-mode*)

(setf *event-mode* nil)

(with-ltk ()
  (default-layout))

(print 'a t)

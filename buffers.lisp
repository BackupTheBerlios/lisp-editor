
(in-package #:lisp-ed)

(defun in-sequence (&rest funs)
  (lambda (&rest args)
    (dolist (f funs)
      (apply f args))))

(defun default-buffer-preset (text)
  (nub-copy-text-bindings text)
  (emacs-like-copy-text-bindings text))

(defmacro let1 (by-name object &body body)
  `(let ((,by-name ,object))
     ,@body))

(defparameter *buffers* nil)

(defun default-layout
    (&key (buffer-preset #'default-buffer-preset))
  (let1 buffer (make-instance 'text)
    (pack buffer :side :top)
    (funcall buffer-preset buffer)
    (push buffer *buffers*))

  (let1 buffer (make-instance 'text)
    (pack buffer :side :left)
    (funcall buffer-preset buffer)
    (push buffer *buffers*)))


(with-ltk ()
  (default-layout))
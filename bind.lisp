
(in-package #:lisp-ed)

(defparameter *event-mode* nil)
(defparameter *events* nil)

(defun assure-event-mode (mode)
  (unless (getf *events* mode)
    (setf (getf *events* mode) (make-hash-table :test 'equalp))))

(defun add-moded-event (mode event fun)
  (assure-event-mode mode)
  (setf (gethash event (getf *events* mode)) fun))

(defun make-event (event fun)
  (lambda (evt)
    (if (null *event-mode*)
      (funcall fun evt)
      (when-with alt (getf *events* *event-mode*)
	(funcall (gethash alt event) case)))))

(defmacro with-binding (obj &body body)
  (with-gensyms (objv)
    `(let ((,objv ,obj))
       (flet ((bind-obj (event fun)
		(bind ,objv event
		      (make-event event (lambda (evt)
					  (declare (ignored evt))
					  (funcall fun ,objv)))))
	      (bind-obj-evt (event fun)
		(bind ,objv event
		      (make-event event (lambda (evt)
					  (funcall fun ,objv evt)))))
	      (bind-evt (event fun)
		(bind ,objv event fun))
	      (bind-fn (event fun)
		(bind ,objv event
		      (make-event event (lambda (evt)
					  (declare (ignored evt))
					  (funcall fun)))))
	      (bind-activate (event fun)
		"Allows other event modes to use it. Currently not clever\
 enough to detect if events are already taken."
		(bind ,objv event
		      (make-event event (lambda (evt)
					  (declare (ignored evt)))))))
	 ,@body))))

(defun rotate-buffer-focus ()
  (when (eql (type-of *focus*) 'text)
    (do ((b *buffers* (cdr b)))
	((null b) nil)
      (when (equalp (car b) *focus*)
	(return (setf *focus*
		      (if-with to (cadr b)
			(setf *focus* to)
			(setf *focus* (car *buffers*)))))))))

(defparameter *focus* nil)

(defun event-mode-bindings (text)
  (with-binding (text)
    (bind-fn "<Control-Key-x>" (lambda () (setf *event-mode* :x)))
    (bind-fn "<Return>" (lambda () (setf *event-mode* nil)))))

(defun selection-bindings (text)
  (with-binding (text)
    (flet ((take-focus (obj)
	     (setf *focus* obj)
	     (takefocus 1)))
      (bind-obj "<ButtonPress-1>" #'take-focus)
      (bind-obj "<ButtonPress-2>" #'take-focus)
      (assure-event-mode :x)
      (add-moded-event :x "<Key-o>" (lambda (case)
				      (declare (ignored case))
				      (rotate-buffer-focus))))))

(defun nub-copy-text-bindings (text)
  (with-binding text
    (bind-obj "<Control-Key-x>" #'cut)
    (bind-obj "<Control-Key-c>" #'copy)
    (bind-obj "<Control-Key-v>" #'paste)))

(defun emacs-like-copy-text-bindings (text)
  (with-binding text
    (bind-obj "<Alt-Key-d>" #'e-delete-word-after)
    (bind-obj "<Alt-Backspace>" #'e-delete-word-before)
    (bind-obj "<Control-Key-k>" #'e-kill)
    (bind-fn "<Alt-Key-y>" #'e-cycle)
    (bind-obj "<Control-Key-y>" #'e-paste)))
  

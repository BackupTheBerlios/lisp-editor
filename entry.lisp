
(in-package #:lisp-ed)

(defparameter *entry* nil "The entry")
(defparameter *entry-mode* nil)

(defparameter *entry-funs*
  (list :default-fun (lambda (str)
		       (declare (type string str))
		       (eval (read-from-string str))
		       (back-from-entry)))
  "Functions associated with completion of entry. :default-fun is for nil.")

(defmacro def-entry-fun ((name &key (evt)) &body body)
  "Defines an entry function."
  (with-gensyms (may-evt)
    `(setf (getf *entry-funs*)
	   (lambda (,(if-use evt may-evt))
	     ,@(when (and (cdr body) (stringp (car body)))
		 (list (car body)))
	     ,@(unless evt
		 `((declare (ignore ,may-evt))))
	     ,@body))))	  

(defun to-entry (from with-mode)
  (declare (ignore from))
  (lambda (evt)
    (declare (ignore evt))
    (setf *entry-mode* with-mode)
    (format-wish "focus ~a" (widget-path *entry*))))

(defun back-from-entry ()
  (format-wish "focus ~a" (widget-path (car *buffers*))))

(defun make-default-entry (&key (alternative-return "<Control-Key-g>"))
  (let ((entry (make-instance 'text :height 1)))
    (bind entry "<Return>"
	  (lambda (evt)
	    (if-let fun (getf *entry-funs* *entry-mode*)
	      (funcall fun evt)
	      (funcall (getf *entry-funs* :default-fun) evt)))
	  :exclusive t)
    (flet ((return-from-entry (&rest evts)
	     (dolist (e evts)
	       (bind entry e (lambda (evt)
			       (declare (ignore evt))
			       (back-from-entry))))))
      (return-from-entry "<Escape>" alternative-return))
    entry))

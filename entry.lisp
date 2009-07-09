
(in-package #:lisp-ed)

(defparameter *entry* nil "The entry")
(defparameter *entry-mode* nil)

(defparameter *entry-funs*
  (list :default-fun (lambda (str)
		       (declare (type string str))
		       (eval (read-from-string str))
		       (back-from-entry)))
  "Functions associated with completion of entry. :default-fun is for nil.")

(defun to-entry (from with-mode)
  (lambda (evt)
    (declare (ignore evt))
    (setf *entry-mode* with-mode)
    (format-wish "focus ~a" (widget-path *entry*))))

(defun back-from-entry ()
  (format-wish "focus ~a" (widget-path (car *buffers*))))

(setf (getf *entry-funs* :open-file)
      (lambda (evt) ;TODO regexpression, looking at filesystem.
	(load-text (car *buffers*) (text *entry*))
	(back-from-entry)))

(setf (getf *entry-funs* :save-file)
      (lambda (evt)
	(save-text (car *buffers*) (text *entry*))
	(back-from-entry)))

(defun make-default-entry (&key (alternative-return "<Control-Key-g>"))
  (let ((entry (make-instance 'text :height 1)))
    (bind entry "<Return>"
	  (lambda (evt)
	    (if-with fun (getf *entry-funs* *entry-mode*)
	      (funcall fun evt)
	      (funcall (getf *entry-funs* :default-fun) evt)))
	  :exclusive t)
    (flet ((return-from-entry (&rest evts)
	     (dolist (e evts)
	       (bind entry e (lambda (evt)
			       (back-from-entry))))))
      (return-from-entry "<Escape>" alternative-return))
    entry))

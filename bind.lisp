
(in-package #:lisp-ed)

(defparameter *event-mode* nil)
(defparameter *events* nil)
(defparameter *event-counter* 0 "Counts some of the events.")

(defun make-event (event fun)
  (lambda (evt)
    (setf- + *event-counter* 1) ;Keep count on these events.
    (if (null *event-mode*)
      (funcall fun evt)
      (when-with alt (getf *events* *event-mode*)
	(when-with fun (gethash event alt)
	  (funcall fun evt))))))

(defmacro with-binding (obj &body body)
  (with-gensyms (objv)
    `(let ((,objv ,obj))
       (flet ((bind-obj (event fun &key append (exclusive t))
		(bind ,objv event
		      (make-event event (lambda (evt)
					  (declare (ignored evt))
					  (funcall fun ,objv)))
		      :append append :exclusive exclusive))
	      (bind-obj-evt (event fun &key append (exclusive t))
		(bind ,objv event
		      (make-event event (lambda (evt)
					  (funcall fun ,objv evt)))
		      :append append :exclusive exclusive))
	      (bind-evt (event fun &key append (exclusive t))
		(bind ,objv event fun
		      :append append :exclusive exclusive))
	      (bind-fn (event fun &key append (exclusive t))
		(bind ,objv event
		      (make-event event (lambda (evt)
					  (declare (ignored evt))
					  (funcall fun)))
		      :append append :exclusive exclusive))
	      (bind-activate (event &key append (exclusive t))
		"Allows other event modes to use it. Currently not clever\
 enough to detect if events are already taken."
		(bind ,objv event
		      (make-event event (lambda (evt)
					  (declare (ignored evt))))
		      :append append :exclusive exclusive)))
	 ,@body))))

(defun assure-event-mode (mode)
  (unless (getf *events* mode)
    (setf (getf *events* mode) (make-hash-table :test 'equalp))))

(defun add-moded-event (mode event fun)
  (assure-event-mode mode)
  (setf (gethash event (getf *events* mode)) fun))

(defun event-mode-bindings (text)
  (with-binding text
    (flet ((end-event-mode (event mode &optional activate)
	     (when activate
	       (bind-activate event))
	     (add-moded-event mode event
	       (lambda (case)
		 (declare (ignored case))
		 (setf *event-mode* nil)))))
      (bind-fn "<Control-Key-x>"
	       (lambda () (setf *event-mode* :x)))

      (end-event-mode "<Control-Key-g>" :x t)
      (end-event-mode "<Key-Escape>" :x t))))

(defun rotate-buffer-focus ()
  (setf- cdr *focus*)
  (when (null *focus*)
    (setf *focus* *buffers*))
  (format-wish "focus ~a" (widget-path (car *focus*))))

(defun selection-bindings (text) ;;Untested
  (with-binding text
    (add-moded-event :x "<Control-Key-o>"
		     (lambda (case)
		       (declare (ignored case))
		       (rotate-buffer-focus)
		       (setf *event-mode* nil)))
    (bind-activate "<Control-Key-o>")))

(defun nub-copy-text-bindings (text)
  (with-binding text
    (bind-obj "<Control-Key-x>" #'cut)
    (bind-obj "<Control-Key-c>" #'copy)
    (bind-obj "<Control-Key-v>" #'paste)))

(defun emacs-like-copy-text-bindings (text)
  (with-binding text
    (bind-obj "<Alt-Key-d>" #'e-delete-word-after)

  ;TODO why the hell not working?
    (bind-obj "<Alt-BackSpace>" #'e-delete-word-before)
    (bind-obj "<Control-BackSpace>" #'e-delete-word-before)
    
    (bind-obj "<Control-Key-k>" #'e-kill)
    (bind-fn "<Alt-Key-y>" #'e-cycle)
    (bind-obj "<Control-Key-y>" #'e-paste)
    (bind-obj "<Control-Key-Y>" #'e-paste-and-cycle)))
  
(defun emacs-like-cursor-motion (text)
  (with-binding text
    (bind-fn "<Control-Key-f>" (lambda ()
				 (move-cursor-pos text 1)))
    (bind-fn "<Control-Key-b>" (lambda ()
				 (move-cursor-pos text -1)))
    (bind-fn "<Alt-Key-f>" ;TODO "" is a word for tk too -_-
      (lambda ()
	(move-cursor-pos 
	 text (length (get-current-word-after-cursor text)))))
    (bind-fn "<Alt-Key-b>" ;TODO doesn't work when at end of word ..
      (lambda ()
	(move-cursor-pos text -1)
	(let ((len (length (get-current-word-before-cursor text))))
	  (move-cursor-pos text (- len)))))))

(defun evaluate-binding (text)
  (with-binding text
    (bind-obj "<Alt-Key-x>"
     (lambda (text) ;TODO incorporate scanning.
       (setf (text *entry*)
	     (format nil "~D"
		     (eval (read-from-string
		     (get-current-expression text)))))))))

;;TODO
;(defun goto-binding (text)
;   "Idea to jump to the function/macro under the cursor."
;Need to be able to parse proper first!
;(defun doc-binding (text)
;   "Idea to jump to the function/macro's information and\
; data."
;Parse proper
;(defun bookmark-binding (text)
; "Some bookmark stuff."
;Figure out what is a good way to do it.

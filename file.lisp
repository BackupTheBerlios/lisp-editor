
(in-package #:lisp-ed)

(defstruct buffer-element
  (buffer-name nil :type string)
  (text "" :type string))

(defparameter *buffer-elements* nil)

(defun find-buffer-el (str slot-name)
  "Finds buffer-element for which the slot name is string= with str."
  (declare (type string str) (type symbol slot-name))
  (find-if (lambda (b)
	     (string= (slot-value b slot-name) str))
	   *buffer-elements*))

(defun buffer-to-elements (&optional (buffer (car *buffers*)))
  (declare (buffer buffer))
  (with-slots (text buffer-name filename) buffer
    (cond*
     (:let b-el (find-buffer-el buffer-name 'buffer-name)
      (setf (slot-value b-el 'text) text
	    (slot-value b-el 'buffer-name) buffer-name
	    (slot-value b-el 'filename filename)))
     (t
      (push (make-buffer-element :text text
	      :buffer-name buffer-name :filename filename)
	    *buffer-elements*)))))

(defun element-to-buffer (el)
  (declare (type buffer-element el))
  (with-slots (file-name buffer-name text) (car *buffers*)
    (setf (slot-value el 'buffer-name) buffer-name
	  (slot-value el 'file-name) file-name
	  (slot-value el 'text) text)))

(setf (getf *entry-funs* :open-file)
      (lambda (evt) ;TODO regexpression, looking at filesystem.
	(cond*
	 (:let b-el (find-buffer-el (text *entry*) 'file-name)
	   (buffer-to-elements)
	   (element-to-buffer b-el))
	 (t
	  (buffer-to-elements)
	  (load-text (car *buffers*) (text *entry*))
	  (with-slots (file-name buffer-name) (car *buffers*)
	    (do ((k 0 (+ k 1)) ;Find a good name.
		 (name file-name (format nil "~D-~D" file-name k)))
		((not(find-buffer-el name 'buffer-name))
		 (setf buffer-name name))))))
	(back-from-entry)))

(setf (getf *entry-funs* :save-file)
      (lambda (evt)
	(save-text (c*ar *buffers*) (text *entry*))
	(back-from-entry)))

(defparameter *buffers* nil)
(defparameter *focus* nil)

(defclass buffer (text)
  ((buffer-name :initform nil :initarg :buffer-name
		:type string)))

(defun add-buffer (&key buffer-preset (side :left) master)
  (let ((buffer (make-instance 'buffer :master master :takefocus t
			       :buffer-name (gensym))))
    (pack buffer :side :left :fill :both :expand t)
    (funcall buffer-preset buffer)
    (push buffer *buffers*)))

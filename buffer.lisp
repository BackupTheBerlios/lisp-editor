
(in-package #:lisp-ed)

(defstruct buffer-element
  (buffer-name "" :type string)
  (file-name "" :type string)
  (text "" :type string))

(defclass buffer (text)
  ((buffer-name :initform "" :initarg :buffer-name :type string)
   (file-name :initform "" :initarg :file-name :type string)))

(defparameter *buffer-elements* nil)

(defun find-buffer-el (str slot-name)
  "Finds buffer-element for which the slot name is string= with str."
  (declare (type string str) (type symbol slot-name))
  (find-if (lambda (b)
	     (string= (slot-value b slot-name) str))
	   *buffer-elements*))

(defun buffer-to-elements (&optional (buffer (car *buffers*)))
  "Add the buffer to elements/update the current data in elements."
  (declare (buffer buffer))
    (cond*
     (:let b-el (find-buffer-el (slot-value buffer 'buffer-name)
				'buffer-name)
      (with-slots (text buffer-name file-name) b-el
	(setf text (text buffer)
	      buffer-name (slot-value buffer 'buffer-name)
	      file-name (slot-value buffer 'file-name))))
     (t
      (push (make-buffer-element :text (text buffer)
	      :buffer-name (slot-value buffer 'buffer-name)
	      :file-name (slot-value buffer 'file-name))
	    *buffer-elements*))))

(defun element-to-buffer (el)
  "But element data in buffer."
  (declare (type buffer-element el))
  (with-slots (file-name buffer-name text) (car *buffers*)
    (setf (slot-value el 'buffer-name) buffer-name
	  (slot-value el 'file-name) file-name
	  (slot-value el 'text) text)))

(defun go-to-file (file-name)
  "Goes to the file in question."
  (cond*
   (:let b-el (find-buffer-el to-file-name 'file-name)
     (buffer-to-elements)
     (element-to-buffer b-el))
   (t
    (buffer-to-elements)
    (load-text (car *buffers*) to-file-name) ;TODO what of inexistance file?
    (with-slots (file-name buffer-name) (car *buffers*)
      (do ((k 0 (+ k 1)) ;Find a good name. (Can't be file name when taken.)
	   (name file-name (format nil "~D-~D" file-name k)))
	  ((not(find-buffer-el name 'buffer-name))
	   (setf buffer-name name)))))))

(defun go-to-line (line)
  "Goes to line number of line."
  (set-cursor-pos (car *buffers*) (format nil "0.~D" line)))

(defun go-to-buffer (buffer-name)
  "Goes to the buffer. nil if didn't exist."
  (when-let b-el (find-buffer-el (text *entry*) 'buffer-name)
    (buffer-to-elements)
    (element-to-buffer b-el)
    t))

(def-entry-fun (:open-file)
  "Open a file, or it's respective buffer."
  (go-to-file (text *entry*))
  (back-from-entry))

(def-entry-fun (:save-file)
  "Save to the given name."
  (save-text (car *buffers*) (text *entry*))
  (back-from-entry))

(def-entry-fun (:to-buffer)
  "Go to the buffer."
  (unless (go-to-buffer (text *entry*))
    (error "TODO message of nonexistence of buffer.")))

(defparameter *buffers* nil)
(defparameter *focus* nil)

(defun add-buffer (&key buffer-preset (side :left) master)
  (let ((buffer (make-instance 'buffer :master master :takefocus t
			       :buffer-name (symbol-name (gensym)))))
    (pack buffer :side side :fill :both :expand t)
    (funcall buffer-preset buffer)
    (push buffer *buffers*)))

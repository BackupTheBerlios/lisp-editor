
(in-package #:ltk)

;;; This replaces the current ltk implementation of this method to remove
;;; the trailing CR that Tk leaves on.
(defmethod text ((text text))
  (format-wish "senddatastring [~a get 1.0 end-1c]" (widget-path text))
  (read-data))

(in-package #:lisp-ed)

(defmacro def-text-fun (name &body body)
  `(defun ,name (text)
     (declare (type text text))
     ,@body))

(def-text-fun get-cursor-pos (text)
  (format-wish "senddatastring [~a index insert]" (widget-path text))
  (read-data))

(def-text-fun get-current-word
  (format-wish
   "senddatastring [~a get \"insert wordstart\" \"insert wordend\"]"
   (widget-path text))
  (read-data))

(def-text-fun get-current-word-after-cursor
  (format-wish
   "senddatastring [~a get \"insert\" \"insert wordend\"]"
   (widget-path text))
  (read-data))

(def-text-fun get-current-word-before-cursor
  (format-wish
   "senddatastring [~a get \"insert wordstart\" \"insert\"]"
   (widget-path text))
  (read-data))

(def-text-fun get-current-line-after-cursor
  (format-wish
   "senddatastring [~a get \"insert\" \"insert lineend\"]"
   (widget-path text))
  (read-data))

(defun forward-from-position (pos count)
  (format nil "~D~D" (subseq pos 0 2)
	             (+ (parse-integer (subseq pos 2)) count)))

(defun insert-text (txt text &rest tags &key (position "insert"))
  (format-wish "~a insert ~a \"~a\" {~{ ~(~a~)~}}"
    (widget-path txt) position (tkescape text) tags)
  txt)

(defun delete-text (txt start end)
  (format-wish "~a delete ~a ~a" (widget-path txt) start end))

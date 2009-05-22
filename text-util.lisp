
(in-package #:ltk)

;;; This replaces the current ltk implementation of this method to remove
;;; the trailing CR that Tk leaves on.
(defmethod text ((text text))
  (format-wish "senddatastring [~a get 1.0 end-1c]" (widget-path text))
  (read-data))

;;; This replaces the current LTK implementation of this method due to a bug
;;; see: http://common-lisp.net/pipermail/ltk-user/2008-September/000304.html
(defmethod place (widget x y &key width height)
  (format-wish "place ~A -x ~A -y ~A~@[ -width ~a~]~@[ -height ~a~]"
    (widget-path widget)
    (tk-number x) (tk-number y)
    (and width (tk-number width))
    (and height (tk-number height)))
  widget)

(in-package #:lisp-ed)

(defun read-data ()
  (ltk:read-data))

(defmacro def-text-fun (name &body body)
  `(defun ,name (text)
     (declare (type text text))
     ,@body))

(def-text-fun get-cursor-pos
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

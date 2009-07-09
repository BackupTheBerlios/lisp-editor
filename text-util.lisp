
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


(defun read-tk-pos (pos-str &key junk-allowed)
  (let ((cnt
	 (loop for ch across pos-str
	       for cnt from 0
	    when (char= ch #\.) return cnt)))
    (values (parse-integer pos-str :end cnt)
	    (parse-integer pos-str :start (+ cnt 1)
			   :junk-allowed junk-allowed))))

(defun get-cursor-pos (text)
  (format-wish "senddatastring [~a index insert]" (widget-path text))
  (read-data))

(defun get-read-cursor-pos (text &key junk-allowed)
  (read-tk-pos (get-cursor-pos text) :junk-allowed junk-allowed))

(defun get-current-word (text)
  (format-wish
   "senddatastring [~a get \"insert wordstart\" \"insert wordend\"]"
   (widget-path text))
  (read-data))

(defun get-current-word-after-cursor (text)
  (format-wish
   "senddatastring [~a get \"insert\" \"insert wordend\"]"
   (widget-path text))
  (read-data))

(defun get-current-line (text)
  (format-wish
   "senddatastring [~a get \"insert linestart\" \"insert lineend\"]"
   (widget-path text))
  (read-data))

(defun get-current-word-before-cursor (text)
  (format-wish
   "senddatastring [~a get \"insert wordstart\" \"insert\"]"
   (widget-path text))
  (read-data))

(defun get-current-line-after-cursor (text)
  (format-wish
   "senddatastring [~a get \"insert\" \"insert lineend\"]"
   (widget-path text))
  (read-data))

(defun get-current-expression
    (text &key (cnt 0) (line (get-current-line text)) (limit 10000)
     (index 1) inline-comment in-string)
  "Gets the current expression on the current line upto where it ends."
  (when (< limit 0)
    (error "Reached limit of reading an expression. Either there is a bug \
in lisp-ed, or your expression is above the limit.")
    (return-from get-current-expression line))
  (do ((i 0 (+ i 1)))
      ((>= i (length line)) nil)
    (flet ((inline-comment ()
	     (do ((j (+ i 1) (+ j 1))) ((>= (+ j 1) (length line)) nil)
	       (when (and (char= (aref line j) #\|)
			  (char= (aref line (+ j 1)) #\#))
		 (setf inline-comment nil)
		 (return))))
	   (in-string ()
	     (do ((j (+ i 1) (+ j 1))) ((>= j (length line)) (setf i j))
	       (when (and (char= (aref line j) #\")
			  (not (char= (aref line (- j 1)) #\\)))
		 (setf i j
		       in-string nil)
		 (return)))))
      (cond
	(inline-comment
	 (inline-comment))
	(in-string
	 (in-string))
	(t
	 (case (aref line i)
	   (#\" (setf in-string t)
		(in-string))
	   (#\# (unless (>= (+ i 1) (length line))
		  (case (aref line (+ i 1))
		    (#\\ (setf- + i 1))
		    (#\| (setf inline-comment t)
			 (inline-comment)))))
	   (#\; (return))
	   (#\( (setf- + cnt 1))
	   (#\) (setf- - cnt 1)))))))

  (cond ((<= cnt 0)
	 line)
	(t
	 (multiple-value-bind (line ch) (get-read-cursor-pos text)
	   (declare (ignore ch))
	   (let ((line (+ line index)))
	     (format-wish
	      "senddatastring [~a get \"~a.1 linestart\" \"~a.1 lineend\"]"
	      (widget-path text) line line)))
	 (format nil "~D~%~D" line
		 (get-current-expression text
		   :inline-comment inline-comment :in-string in-string
		   :index (+ index 1) :line (read-data)
		   :cnt cnt :limit (- limit 1))))))

(defun move-cursor-pos (text count)
  (format-wish "~a mark set insert \"insert ~a~ac\""
    (widget-path text)
    (if (> count 0) #\+ #\-) (if (> count 0) count (- count))))

(defun forward-from-position (pos count)
  "TODO WARNING only on a single line."
  (multiple-value-bind (line ch) (get-read-cursor-pos text)
    (format nil "~D.~D" line (+ ch count))))

(defun insert-text (txt text &rest tags &key (position "insert"))
  (format-wish "~a insert ~a \"~a\" {~{ ~(~a~)~}}"
    (widget-path txt) position (ltk:tkescape text) tags)
  txt)

(defun delete-text (txt start end)
  (format-wish "~a delete ~a ~a" (widget-path txt) start end))


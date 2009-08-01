;TODO make it optionally scrolled.
(defclass scrolled-text2 (frame)
  ((textbox :type text)
   (hscroll :type (or null scrollbar))
   (vscroll :type (or null scrollbar))))

(defun make-scrolled-text
    (&key want-h (want-v t) (st (make-instance 'scrolled-text2)))
  (with-slots (textbox hscroll vscroll) st
    (setf hscroll (when want-h
		    (make-scrollbar st :orientation "horizontal"))
	  vscroll (when want-v (make-scrollbar st)))
    (setf textbox (make-instance 'text :master st
		     :xscroll (when want-h hscroll)
		     :yscroll (when want-v vscroll)))
    (grid textbox 0 0 :sticky "news")
    (when want-h
      (grid hscroll 1 0 :sticky "we")
      (configure hscroll "command"
		 (format nil "~D xview" (widget-path textbox)))
      (configure textbox "xscrollcommand"
		 (format nil "~D set" (widget-path st))))
    (when want-v
      (grid vscroll 0 1 :sticky "ns")
      (configure vscroll "command"
		 (format nil "~D yview" (widget-path textbox)))
      (configure textbox "yscrollcommand"
		 (format nil "~D set" (widget-path vscroll)))))
  (grid-columnconfigure st 0 :weight 1)
  (grid-columnconfigure st 1 :weight 0)
  (grid-rowconfigure st 0 :weight 1)
  (grid-rowconfigure st 1 :weight 0))


(with-ltk ()
  (pack (make-scrolled-text)))

(defmethod text ((self scrolled-text2))
  (text (textbox self)))

(defmethod (setf text) (new-text (self scrolled-text2))
  (setf (text (textbox self)) new-text))

(defgeneric insert-object (txt object))
(defmethod insert-object ((txt scrolled-text2) obj)
  (format-wish "~a window create end -window ~a" (widget-path (textbox txt)) (widget-path obj))
  txt)

(defgeneric see (txt pos))
(defmethod see ((txt scrolled-text2) pos)
  (format-wish "~a see ~a" (widget-path (textbox txt)) pos)
  txt)

(defmethod see ((lb listbox) pos)
  (format-wish "~a see ~a" (widget-path lb) pos)
  lb)

(with-ltk ()
  (let ((pane (make-instance 'labelframe :name "ni" :text "m")))
    (pack pane)
    (flet ((entry ()
	     (let ((out (make-instance 'entry :master pane)))
	       (bind out ""
		     out
      (dotimes (k 4) (pack (entry))))))
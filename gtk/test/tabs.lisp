(in-package :gtk-stuff)

(defun tabbed ()
  (gtk:within-main-loop
    (gtk:widget-show
     (tabs-like
      (mapcar (lambda (s) ;TODO figure out how to light up the correct ones.
		(list (make-instance
		       'gtk:toggle-button :label (format nil "Tab-~D" s))
		      (lambda ()
			(make-instance
			 'gtk:button :label (format nil "Tab-~D" s)))))
	      '(a b d 35 gd sgds 64))
      :tabs-dir :v))))

(tabbed)

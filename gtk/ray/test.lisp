(defpackage :test-access
  (:use :cl :gtk-stuff :gdk-pixel-access)
  (:export))

(in-package :test-access)

(gtk:within-main-loop
  (gtk:widget-show
   (let ((image (make-instance 'gdk:gdk-image)))
     (
     (make-instance 'list-window
		    :widget (make-instance 'gtk:image :image image)))))

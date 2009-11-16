(in-package :cl-user)

(defpackage :gdk-pixel-access
  (:use :cl :cffi :gdk :gobject)
  (:export gdk-image-xy gdk-image-pix))

(in-package :gdk-pixel-access)

(defcfun gdk_image_put_pixel :void
  (image (g-object gdk-image)) (x :int) (y :int)
  (pixel :uint32))

(defcfun gdk_image_get_pixel :uint32
  (image (g-object gdk-image)) (x :int) (y :int))

(defun gdk-image-xy (image x y)
  (gdk-image-get-pixel image x y))

(defun (setf gdk-image-xy) (to image x y)
  (gdk-image-put-pixel image x y to)
  to)

(defun gdk-image-pix (image pos)
  (gdk-image-pix image (aref pos 0) (aref pos 1)))
(defun (setf gdk-image-pix) (to image pos)
  (setf (gdk-image-pix image (aref pos 0) (aref pos 1)) to))

(declaim (inline gdk-image-xy (setf gdk-image-xy)
		 gdk-image-pix (setf gdk-image-pix)))

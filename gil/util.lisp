;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;;

(cl:in-package :cl)

(defpackage gil-create-util
  (:use :common-lisp :gil :gil-share)
  (:export timestep *timestamp*)
  (:documentation "For creating stuff."))

(in-package :gil-create-util)

(defvar *timestamp* (get-universal-time))
(defun timestamp (&optional (universal-time *timestamp*))
  (multiple-value-bind
	(second minute hour date month year day daylight-p zone)
      (decode-universal-time universal-time)
    (declare (ignore date daylight-p zone))
    (format nil "~D:~D:~D ~D-~D-~D" 
	    hour minute second day month year)))

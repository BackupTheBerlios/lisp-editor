;;
;;  Copyright (C) 2010-03-01 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :plist-slot
  (:use :common-lisp)
  (:export plist-slot pslot)
  (:documentation "A setfable function pslot that uses the slot 'pslot as\
 plist. And a class plist-slot with the slot.(to derive from)"))

(in-package :plist-slot)

(defclass plist-slot ()
  ((pslot :initarg :pslot :initform nil :type list))
  (:documentation "Adds slot with plist, which can be accessed with pslot"))

(defun pslot (of name)
  "Read the plist of pslot at name."
  (getf (slot-value of 'pslot) name))
(defun (setf pslot) (to of name)
  "Set the plist of pslot at name"
  (setf (getf (slot-value of 'pslot) name) to))

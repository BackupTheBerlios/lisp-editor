;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(defpackage :gil-index
  (:use :common-lisp :gil :gil-share :gil-contents)
  (:export)
  (:documentation "Uses indexed declared notable words, makes a page\
 linking back to them."))

(in-package :gil-index)

;;TODO

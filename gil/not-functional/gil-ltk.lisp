;;
;;  Copyright (C) 2010-03-01 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl)

(defpackage gil-ltk
  (:use :common-lisp :generic :gil :gil-share :ltk)
  (:documentation "TODO Latex output of General Interface Library/Language.
Goes by symbol :tex"))

(in-package :gil-ltk)

(make-gil-definer :ltk def-gil-method def-gil-method*)

(def-gil-method i-glist :p ((list list))
  

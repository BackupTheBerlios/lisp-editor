(cl:in-package :cl)

(defpackage gil-ltk
  (:use :common-lisp :generic :gil :gil-share :ltk)
  (:documentation "TODO Latex output of General Interface Library/Language.
Goes by symbol :tex"))

(in-package :gil-ltk)

(make-gil-definer :ltk def-gil-method def-gil-method*)

(def-gil-method i-glist :p ((list list))
  

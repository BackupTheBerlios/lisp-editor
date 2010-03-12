
(cl:in-package :cl-user)

(use-package '(:asdf))

(defsystem :log
  (:description "Very basic logger.")
  (:depends-on :generic :cl-fad)
  (:components
   (:file "log")))

(defsystem :gil-log
;  (:description "TODO")
  (:depends-on :log :gil)
  (:components "gil-log"))

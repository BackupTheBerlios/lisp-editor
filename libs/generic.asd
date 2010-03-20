
(cl:in-package :cl-user)

(use-package '(:asdf))

(defsystem :generic
  :depends-on (:alexandria)
  :components ((:file "generic")))


(cl:in-package :cl-user)

(use-package '(:asdf))

(defsystem :log
  :description "Logs files, when they change, allows to store."
  :serial t
  :depends-on (:alexandria :cl-fad)
  :components
  ((:file "log")))

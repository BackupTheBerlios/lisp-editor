
(cl:in-package :cl-user)

(use-package '(:asdf))

(defsystem :store-log-test
    :depends-on ((:alexandria :cl-store :store-log :cl-fad
		  :generic :lisp-ed-path-stuff))
    :components ((:file "store-log")))

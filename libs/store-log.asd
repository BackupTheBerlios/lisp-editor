
(cl:in-package :cl-user)

(use-package '(:asdf))

(defsystem :store-log
    :description "Uses cl-store to keep track of files from a single\
 file, denoting when they're encountered/first added.

Use cl-store's store and restore read/write the log.
Extend by (defmethod store :around ((log newtype) (to (eql :default))).. 
and (defrestore-cl-store (newtype log) .."
    :depends-on (:alexandria :cl-store :cl-fad :lisp-ed-path-stuff)
    :components ((:file "store-log")))

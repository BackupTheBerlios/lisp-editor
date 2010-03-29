;;
;;  Copyright (C) 2010-03-01 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(use-package '(:asdf))

(defsystem :expression-scan
  :description 
  "Can create and use expression-hook to obtain information about code.
Any s-expression can be tracked. (So macros and functions can be tracked."
  :serial t
  :depends-on (:alexandria :expression-hook :cl-fad)
  :components ((:file "expression-scan")))


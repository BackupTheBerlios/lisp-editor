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

(defsystem :expression-hook
  :description "Macroexpands code purely by itself. *expression-hook* continues it,
 and must call further expand-hook.
Used for gathering information on code autodoc via expression-scan."
  :serial t
  :components ((:file "package-stuff")
	       (:file "expression-hook"))
  :depends-on (:generic :denest))

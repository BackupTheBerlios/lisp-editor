
(cl:in-package :cl-user)

(use-package '(:asdf))

(defsystem :autodoc
  :description "Tools to autodocument things based on CL data,\
 information gathered with expression-scan, which can get information from \
any s-expression, extra information can also be added."
  :serial t
  :depends-on (:generic :denest :expression-scan :gil)
  :components ((:file "autodoc")))

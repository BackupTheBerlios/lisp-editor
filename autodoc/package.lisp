
(defpackage :lisp-editor-autodoc
  (:use :common-lisp :generic :denest
	:expression-hook :expression-scan
	:gil :gil-vars :gil-share :gil-style :gil-read
	:gil-autodoc)
  (:documentation "Package from which to define additional\
 autodocumentation code specific to this project.

The directory mirrors that of the project."))

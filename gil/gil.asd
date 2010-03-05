

(cl:in-package :cl-user)

(use-package '(:asdf))

(defsystem :gil
  :description "General Interface Library/Language. Attempt to make a single\
 system for different kinds of interface and markup.
Probably good idea to use combined with denest."
  :serial t
  :depends-on (:generic :denest)
  :components (;Base stuff.
	       (:file "gil") ;Base gil defgenerics.
	       (:file "gil-share") ;Some tools, and variables.
	       (:file "gil-style")
	       (:file "gil-read") ;Syntax for data-like use. TODO improve.
	       ;Tool to create GIL code.
	       (:file "gil-info") ;produce a table of contents.
	       ;Outputs.
	       (:module "output"
		:serial t
		:components
		((:file "gil-html")
		 (:file "gil-txt")))
               ;User aids.
	       (:file "gil-user")
;(:file "gil-latex")
              ;Tools about it. (autodoc considered separate.)
;               (:module "tools"
;                :serial t
;		:components
;		((:file "contents")))
	       ))

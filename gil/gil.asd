

(cl:in-package :cl-user)

(use-package '(:asdf))

(defsystem :gil
  :description "General Interface Library/Language. Attempt to make a single\
 system for different kinds of interface and markup.
Probably good idea to use combined with denest."
  :serial t
  :depends-on (:generic :alexandria :denest :log :cl-fad
			:lisp-ed-path-stuff)
  :components (;Base stuff.
	       (:module "specify"
		:serial t
                :components
   	        ((:file "gil") ;Base gil defgenerics.
		 (:file "vars")
		 (:file "comms")
 	         (:file "share") ;Some tools, and variables.
	         (:file "style")))
	       (:file "read") ;Syntax for data-like use. TODO improve.
	       ;Tool to create GIL code.
	       (:file "info") ;produce a table of contents.
	       ;(:file "util")
	       ;Outputs.
	       (:module "output"
		:serial t
		:components
		((:file "util")
		 (:file "html")
		 (:file "txt")
		 (:file "latex")))
               ;User aids.
	       (:file "user")
              ;Tools about it. (autodoc considered separate.)
               (:module "tools" 
                :serial t
;TODO damned asd has problems where load has _no indications_..
		:components
		((:file "contents")
;		 (:file "log")
		 )
		)
	       ))


;;Load everything needed for code scanning.
(require :iterate)
(load "generic.lisp")

(load "graph/downgraph.lisp")
(load "graph/code-scan.lisp")

;;Enable scanning.
(setq *macroexpand-hook* #'code-scan:scanning-macrohook)

;;Load everything.(While scanning)
(setq *not-code-scan* t)
(load "load.lisp")
(setq *not-code-scan* nil)

;;Disable scanning.
(setq *macroexpand-hook* #'funcall)

;;Document.(Earlier load should have gotten autodocumentation.
(in-package :autodoc) ;For convenience.

(let (*pages-list*
      (packages '(:generic :lisp-ed :down-graph :down-graph-draw
		  :code-scan :autodoc :mk-website)))
  (dolist (p packages)
    (setf (gethash (find-package p) *package-locations*)
	  (list "" (format nil "../~D/"
			   (string-downcase (symbol-name p))))))
  (dolist (p packages)
    (let ((*directory* (format nil "doc/autodoc/~D/"
			       (string-downcase (symbol-name p)))))
      (autodoc :packages (list p)
	       :page-maker #'page-maker-each-own-file
	       :page-maker-final #'page-maker-contents-final)
      (let ((*links-inside-file* t))
	(autodoc
	 :packages (list p)
	 :page-maker #'page-maker-list
	 :page-maker-final
	 (lambda ()
	   (page-maker-with-contents-final :one-page t)))))))

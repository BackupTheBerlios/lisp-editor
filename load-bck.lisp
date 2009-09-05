
(require :iterate)
(require :ltk)

(defvar *not-code-scan* nil)

(load "generic.lisp")
(load "package-stuff.lisp")

;Graph(drawing) and code scanning.
(load "graph/downgraph.lisp")
(load "graph/downgraph-draw.lisp")
(unless *not-code-scan*
  (load "graph/code-scan.lisp"))
;Website and autodocumentation.
(require :lml2)
(load "mk-doc/mk-website.lisp")
(load "mk-doc/autodoc.lisp")

;Editor stuff.
(load "ed.lisp")

(load "text-util.lisp")
(load "text-actions.lisp")

(load "bind.lisp")
;(load "code-bind.lisp") ;TODO get working again.
(load "file-bind.lisp")

(load "entry.lisp")
(load "buffer.lisp")

(load "buffers.lisp")

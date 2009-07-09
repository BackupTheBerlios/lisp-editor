
(require :iterate)
(require :ltk)

(load "downgraph.lisp")
(load "downgraph-draw.lisp")
(load "code-scan.lisp")

(setq code-scan:*observe-packages* '(:code-scan))

(setq *macroexpand-hook* #'funcall)
(setq *macroexpand-hook* #'code-scan:scanning-macrohook)

(macroexpand '
(defun sqr (x) (* x x)))

(code-scan:stop-here-p 'sb-int:named-lambda)


(special-operator-p 'truly-the)

(cadr code-scan:*unrecognized*)


(macroexpand '(case x
	       (1 2) (3 5) (7 1)))


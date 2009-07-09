(defmacro koekie (x)
  `(* ,x ,x))

(let ((
      (*observe-packages* :all));(list "CL-USER")))
  (macroexpand '(defun lala (x) (+ x 4)))
;  (scan-expanded
;   (macroexpand

(setq *macroexpand-hook* #'funcall)

(in-package :code-scan)

(setq *functions* (make-hash-table)
      *function-links* (list nil))

(setq *macroexpand-hook* #'scanning-macrohook)

(defun sum (list)
  (let ((sum 0))
    (dolist (el list)
      (setf sum (+ el sum)))))

;  (observe-symbol-p 'sqr))




(macro-function 'declare)

(defun sqrsumsqr (x y) 1)

(sqrsumsqr 4 4)

(macro-function 'progn)

(print 'a)

(macroexpand '(dolist (el list)
	       (setf sum (+ el sum))))

(macro-function 'case)

(symbol-function 'a)

(loop for el in '(


) collect (macro-function el))


(special-operator-p 'cond)

(function (progn 'sum))


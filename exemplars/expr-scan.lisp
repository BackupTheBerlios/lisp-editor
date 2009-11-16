(in-package :cl-user)

(defpackage :exemplars-of-expr-scan
  (:use :common-lisp :example :expr-scan)
  (:export code-scanner-ways |scan file| |scan loaded file|
	   |scan expression|))

(in-package :exemplars-of-expr-scan)

(def-example code-scanner-ways (ways)
  "A couple of ways to scan code:"
  ways)

(def-example |scan file| (file)
  "Scan file, not following LOAD"
  (scan-file file))

(def-example |scan loaded file| (file)
  "Scan file, not following LOAD"
  (scan-loadfile file))

(def-example |scan expression| (expr)
  "Scan an expression."
  (scan-expr expr))

(add-arg-info-example 'code-scanner-ways 'ways
  (mapcar #'get-example
	  '(|scan file| |scan loaded file| |scan expression|)))

(add-arg-info-example '|scan file| 'file (list '(:file)))
(add-arg-info-example '|scan loaded file| 'file (list '(:file)))

(add-arg-info-example '|scan expression| 'expr (list '(:code)))

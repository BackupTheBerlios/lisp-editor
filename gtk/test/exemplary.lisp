(in-package :cl-user)

(load-example :expr-scan)

(defpackage :exemplary-test
  (:use :common-lisp :exemplary :gtk-stuff 
	:exemplars-of-expr-scan) ;Currently testing with this.
  (:export test))

(in-package :exemplary-test)

(defun test ()
  (gtk:within-main-loop
    (gtk:widget-show
     (make-instance 'list-window :title "test"
       :widgets (list (make-instance 'gtk-example :shadow-type :none :shadow :none
			:example (example:get-example 'code-scanner-ways)))))))

(test)


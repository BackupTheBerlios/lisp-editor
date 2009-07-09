
(defpackage :code-scan
  (:use #:common-lisp #:iterate #:down-graph)
  (:export *functions* *cur-file* *cur-line* *in-type* *in-fun*
	   *function-links* *observe-packages*
	   *function-tracker*
	   observe-symbol-p scanning-macrohook))

(in-package :code-scan)

(defparameter *functions* (make-hash-table))

(defstruct function-tracker
  (from-line 0  :type integer)
  (from-file "" :type string)
  (name nil :type (or null symbol))
  (args '(:unknown-args) :type list))

(defmacro if-let (var cond if-t if-f)
  `(let ((,var ,cond))
     (if ,var ,if-t ,if-f)))

(defun track-function
    (name args &key (from-line *cur-line*) (from-file *cur-file*))
  (setf (gethash name *functions*)
	(make-function-tracker
	 :from-line from-line :from-file from-file
	 :name name :args args)))

(defparameter *cur-file* "" "Current file.")
(defparameter *cur-line* 0 "Current line.")

(defparameter *in-type* nil
  "Last type that was registered.")
(defvar *in-fun* nil
  "Last function that was registered.")

(defparameter *unrecognized* nil ;TODO confirm functions as functions.
  "All the macro/function name that are not functions, not ")

(defparameter *function-links* (list nil)
  "Holds a list of how functions are related in the fashion of down-graph.")

(defparameter *observe-packages* :all
  "Which packages are observed. Defaultly :all, otherwise list of\
 packages.")

(defparameter *stop-with-packages*
  (list :sb-impl :sb-int :sb-c) ;TODO SBCL specific.
  "Packages, when encountered, it stops scanning.")

(defun find-string-equal (string list)
  (declare (type list list))
  (find-if (lambda (sym) (string= (symbol-name sym) string)) list))

(defun stop-here-p (sym)
  "Stop when see one of the *stop-with-packages*"
  (declare (type symbol sym))
  (find-string-equal (package-name(symbol-package sym))
		     *stop-with-packages*))

(defun observe-symbol-p (sym)
  "Returns whether the symber is being observed under current\
 *observe-packages*." 
  (declare (type symbol sym))
  (cond
;    ((not(symbolp sym)) nil)
    ((eql *observe-packages* :all) t)
    (t
     (find-string-equal (package-name(symbol-package sym))
			*observe-packages*))))

(defun may-register (symbol)
  (declare (type symbol symbol))
  (when (and *in-fun* ;Register macro, go back to expanding.
	     (observe-symbol-p *in-fun*)
	     (observe-symbol-p symbol))
    (add-link *function-links* *in-fun* symbol))
  t)

(defun scan-expanded-list (list)
  (dolist (c list) ;And keep doing it's thing.
    (scan-expanded c))
  t)

(defun scan-expanded (code)
  (cond
    ((or (not(listp code)) (null code))
     t) ;Doesn't handle variables yet.
    ((listp (car code))
     (unless (eql (caar code) 'lambda) ;Why, lisp, whhhyyyy??
       (error "Wut a list in first element?~% ~D" code))
     (scan-expanded (car code))
     (scan-expanded-list (cdr code)))
    ((stop-here-p (car code))
     t)
   ;Register and leave it for the macrohook to pick up.
    ((macro-function (car code))
     (may-register (car code)))
    ((case (car code) ;Note that where they are here doesn't mean that their
;arguments are exactly like that.
     ;These currently do nothing.
       ((declare defpackage in-package quote go)
	t)
       ((progn tagbody locally
	 multiple-value-call multiple-value-prog1 unwind-protect)
	 (scan-expanded-list (cdr code)))
       (load-time-value
	(scan-expanded-list (butlast (cdr code))))
       (progv
	 (destructuring-bind (vars vals &body body) (cdr code)
	   (scan-expanded vars)
	   (scan-expanded vals)
	   (scan-expanded-list body)))
       ((let let* labels)
	(destructuring-bind ((&rest vars) &body body) (cdr code)
	  (dolist (v vars)
	    (when (listp v)
	      (scan-expanded (cadr v))))
	  (scan-expanded-list body)))
       ((flet macrolet)
	(destructuring-bind ((&rest funs) &body body) (cdr code)
	  (dolist (f funs)
	    (scan-expanded-list (cddr f))) ;TODO &key, &optional
	  (scan-expanded-list body)))
       (symbol-macrolet
	(destructuring-bind ((&rest macs) &body body) (cdr code)
	  (dolist (m macs)
	    (scan-expanded-list (cdr m)))
	  (scan-expanded-list body)))
       ((eval-when catch throw block)
	(scan-expanded-list (cddr code)))
       (if
	(destructuring-bind (cond if-t &optional if-f) (cdr code)
	  (scan-expanded cond)
	  (scan-expanded if-t)
	  (scan-expanded if-f)))
       (function ;I don't get why people throw lists in there..
	(unless (listp (cadr code))
	  (may-register (cadr code)))
	t)
       ((return-from the)
	(scan-expanded (caddr code)))
       (setq
	(do ((c (cddr code) (cddr c)))
	    ((null c) t) ;Note no handling of variables here.
	  (scan-expanded (car c)))
	t))
     t)
    ((special-operator-p (car code)) ;Unrecognized special operator?!
     (unless (find (car code) *unrecognized*)
       (push (car code) *unrecognized*))
     t)
    (t ;Must be function then.
     (may-register (car code))
     (scan-expanded-list (cdr code)))))

(defun scanning-macrohook (expander form env)
  (let ((res (funcall expander form env))) ;Macroexpand.
    (cond
      ((case (car form) ((defun defmacro) t))
       (destructuring-bind (type name (&rest args) &body body) form
	 (when (observe-symbol-p name)
	   (track-function name args)) ;Observe function, if required.
	;TODO scan &optional and &key
	 (setq *in-type* type
	       *in-fun* name)
	 (scan-expanded-list body))
       res)
      ((stop-here-p (car form)) ;Attempts to stop.
       (let ((*macroexpand-hook* #'funcall))
	 (macroexpand res)))
      (t
       (scan-expanded res) ;scan it.
       res)))) ;Return.

#|
 (defun scan-expanded (code)
  "Scans expanded code code for dependencies."
  (when (or (null code) (not (listp code)))
    (return-from scan-expanded code))
  
  (when (and *in-fun*
	     (observe-symbol-p *in-fun*)
	     (observe-symbol-p (car code)))
    (add-link *function-links* *in-fun* (car code)))
  
  (when (listp (car code))
    (warn "Expected symbol at start of this:~%~D" code)
    (return-from scan-expanded code)) 
  
  (when (macro-function (car code))
    (return-from scan-expanded code))
;      (scanning-macrohook (macro-function (car code)) code nil)))
  
  (case (car code)
    ((let let*)
     (destructuring-bind ((&rest vars) &body body) (cdr code)
       (dolist (v vars)
	 (when (listp v)
	   (scan-expanded (cadr v))))
       (scan-expanded-list body)))
    ((flet)
     (destructuring-bind ((&rest funs) &body body) (cdr code)
       (dolist (f funs)
	 (scan-expanded-list (cddr f))) ;TODO &key, &optional
       (scan-expanded-list body)))
    ((case)
     (scan-expanded (cadr code))
     (dolist (case (cddr code))
       (scan-expanded-list (cdr case))))
    ((cond)
     (dolist (el (cdr code))
       (dolist (c el)
	 (scan-expanded-list c))))
    ((declare defpackage in-package))
    ((eval-when)
     (scan-expanded-list (cddr code)))
    (t
     (scan-expanded-list (cdr code))))
  code)

 (defun scanning-macrohook (expander form env)
  "Macrohook that also scans the code. Note that this function is made such\
that it dances around where things are variables."
  (flet ((scanned (code)
	   (let ((result (funcall expander code env)))
	     (scan-expanded result)
	     result)))
  (case (car form)
    ((defmacro defun)
     (let ((*in-fun* (cadr form))
	   (*in-type* (car form)))
       (destructuring-bind (name (&rest args) &body body) (cdr form)
	 (when (observe-symbol-p name)
	   (track-function name args))
	;TODO scan &optional and &key
	 (scan-expanded-list body)
	 (funcall expander form env))))
    ((in-package defpackage) ;Not done for now.
     (funcall expander form env))
    ((let flet let* progn)
     (error "Code walker not designed to treat these as 'actual' macros.\
 (This might be implementation dependend."))
    (t
     (scanned form)))))
|#
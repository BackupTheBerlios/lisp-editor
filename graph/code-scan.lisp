
(defpackage :code-scan
  (:use :common-lisp :generic :iterate :down-graph)
  (:export *functions* *cur-pos* *in-type* *in-fun*
	   *function-links* *observe-packages* *ignore-packages*
	   *function-tracker*
	   enable-code-scan disable-code-scan
	   observe-symbol-p scanning-macrohook
	   track-input
	   
	   function-tracker tracked-inputs
	   from-pos data name args track-fun tracked-inputs))

(in-package :code-scan)

(defvar *functions* (make-hash-table)
  "Hash table with  all the tracked functions in it.")
(defparameter *track-when-encounter* nil
  "list of things to add a tracker to when they are encountered.")

(defun track-input (on-symbol track-method)
  "Stores something of when you use the given macro/function.

The function takes the function/macro call s-expression as argument.

if on-symbol is a list, the first one is the package name and the second\
 the function/macro name, this enables you to define tracking methods for\
 not-yet defined functions/macros."
  (cond
    ((symbolp on-symbol)
     (if-let fun-track (gethash on-symbol *functions*)
       (setf (slot-value fun-track 'track-fun) track-method)
       (error "Tried to track a function that didn't exist.\
 (Try giving it a package-name symbol-name pair)")))
    ((listp on-symbol)
     (let ((package-str (if (stringp (car on-symbol)) (car on-symbol)
			    (symbol-name (car on-symbol))))
	   (symbol-str (if (stringp (cadr on-symbol)) (cadr on-symbol)
			   (symbol-name (cadr on-symbol)))))
       (push (list package-str symbol-str track-method)
	     *track-when-encounter*)))))

(defclass tracked-inputs ()
  ((from-pos :initform :unknown :initarg :from-pos
         :documentation "Where the use of the macro/function was found.")
   (data :initform nil :initarg :data
	 :documentation "Data that is being tracked")))

(defclass function-tracker ()
  ((from-pos :initform :unknown :initarg :from-pos)
   (name :initform :unknown :initarg :name :type symbol)
   (type :initform :unknown :initarg :type :type symbol)
   (args :initarg :args :initform '(:unknown-args) :type list)
   (track-fun :initarg :track-fun :initform nil
	      :type (or null (function (list) t))
	      :documentation "Function of which the output is put in\
 tracked-inputs.")
   (tracked-inputs :initform nil :type list)
   (data :initform nil :documentation "Other data that can be put in.")) 
  (:documentation "Records functions and macros, their arguments and what\
 *cur-pos* says on when they were recorded.."))

(defparameter *cur-pos* :unset
  "Current position, likely filename, linenumber.")

(defun track-function (name args type &key (from-pos *cur-pos*))
  "Enables the function to be tracked."
  (let (track-fun)
    (setf *track-when-encounter*
	  (delete-if
	   (lambda (el) ;See if there is something waiting to track it.
	     (when (and (string= (car el)
				 (package-name (symbol-package name)))
		(string= (cadr el) (symbol-name name)))
	       (setf track-fun (caddr el))
	       t))
	   *track-when-encounter*))
    (setf (gethash name *functions*)
	  (make-instance 'function-tracker :from-pos from-pos :type type
			 :name name :args args :track-fun track-fun))))

(defparameter *in-type* nil
  "Last type that was registered.")
(defvar *in-fun* nil
  "Last function that was registered.")

(defparameter *unrecognized* nil
  "All the macro/function name that are not functions, not ")

(defvar *function-links* (list nil)
  "Holds a list of how functions are related in the fashion of down-graph.
TODO change the graph code such that it can use the data if inside the\
 function-tracker class.")

(defparameter *observe-packages* :all
  "Which packages are observed. Defaultly :all, otherwise list of\
 packages.")

(defparameter *ignore-packages*
  (list :sb-impl :sb-int :sb-c :sb-pcl :sb-kernel) ;TODO SBCL specific.
  "Packages, when encountered, it stops scanning.")

(defun find-string-equal (string list)
  "Find the string that is equal to given in the list."
  (declare (type list list) (type string string))
  (find-if (lambda (sym) (string= (symbol-name sym) string)) list))

(defun stop-here-p (sym)
  "Stop when see one of the *stop-with-packages*"
  (declare (type symbol sym))
  (find-string-equal (package-name(symbol-package sym))
		     *ignore-packages*))

(defun observe-symbol-p (sym)
  "Returns whether the symber is being observed under current\
 *observe-packages*." 
  (declare (type symbol sym))
  (cond
    ((eql *observe-packages* :all) t)
    (t
     (find-string-equal (package-name(symbol-package sym))
			*observe-packages*))))

(defun may-track-inputs (code)
  "Tracks the inputs of functions/macros, if required."
  (when-let tf (gethash (car code) *functions*)
    (with-slots (track-fun tracked-inputs) tf
      (when track-fun ;Track data in function/macro.
	(push (make-instance 'tracked-inputs :from-pos *cur-pos*
			     :data (funcall track-fun code))
	      (slot-value tf 'tracked-inputs))))))

(defun may-register (code)
  "Registers macro/function if observed and have a context."
  (declare (type (cons symbol list) code))
  (when (and ;Register macro, go back to expanding.
	     (observe-symbol-p *in-fun*)
	     (observe-symbol-p (car code)))
    (add-link *function-links* *in-fun* (car code))
    (may-track-inputs code))
  t)

(defun scan-arg-list (args)
  "Scans argument list for defaults."
  (let (state) ;Scan &optional and &key ;TODO tag them as such?
    (dolist (a args) ;TODO untested.
      (case a
	(&optional (setf state :optional))
	(&key      (setf state :key))
	(&rest nil)
	(t
	 (when (and state (listp a))
	   (scan-expanded (cadr a))))))))

(defun scan-expanded-list (list)
  "Just shortcut."
  (dolist (c list) ;And keep doing it's thing.
    (scan-expanded c))
  t)

(defun scan-expanded (code)
  "Scans expanded code, stops as STOP-HERE-P indicates, and at macros.\
 (Which trigger the hook)"
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
     (may-register code))
    ((case (car code) ;Note that where they are here doesn't mean that their
;arguments are exactly like that. (Only such that they're handled.)
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
	    (destructuring-bind (name (&rest args) &body body) f
	      (scan-arg-list args)
	      (scan-expanded-list body)))
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
;	(unless (listp (cadr code))
;	  (may-register (cadr code)))
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
;TODO Need to figure out how to see macrolet macros.
    ((not(fboundp (car code)))
     t);(scan-expanded-list (cdr code)))
    (t ;Must be function then.
     (may-register code)
     (scan-expanded-list (cdr code)))))

(defun scanning-macrohook (expander form env)
  "Macrohook that scans the code. Notably their connections."
  (let ((res (funcall expander form env))) ;Macroexpand.
    (cond
      ((case (car form) ((defun defmacro) t))
       (destructuring-bind (type name (&rest args) &body body) form
	 (when (observe-symbol-p name) ;Observe function, if required.
	   (track-function name args (car form)))
	 (scan-arg-list args)
	 (setq *in-type* type ;Set context.
	       *in-fun* name)
	 (scan-expanded-list body))) ;Scan body.
      ((stop-here-p (car form)) ;Attempts to stop.
       nil)
      (t
       (scan-expanded res))) ;scan it.
    res)) ;Return.

(defun enable-code-scan ()
  "Enables the code scanning.(disable-... disables it.)
Of course, this is just changing cl:*macroexpand-hook* !"
  (setq *macroexpand-hook* #'scanning-macrohook))

(defun disable-code-scan ()
  "Disables the code scanning."
  (setq *macroexpand-hook* #'funcall))

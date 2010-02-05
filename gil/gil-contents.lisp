
(cl:in-package :cl-user)

(defpackage gil-contents
  (:use :common-lisp :generic :denest :gil :gil-share :gil-read)
  (:export use-contents))

(in-package :gil-contents)

(defparameter *contents* nil)

(defvar *register-first* nil)

(defun gather-contents (&rest files)
  (let ((*contents* nil) ;TODO ability to hide things from contents.
	(*lang* :contents))
    (mapcar (curry #'gil-read::gil-execute :funcall nil)
	    files)
    (reverse *contents*)))

;;TODO seems like a general-purpose function to me..
(defun rank (list &key (compare #'<=))
  "Makes a hierarchical tree of the elements preserving order, based on\
 comparing."
  (when (null list)
    (return-from rank nil))
  (gen:if-let i (position-if (gen:curry compare (car list)) (cdr list))
    (cons (cons (car list)
		(rank (subseq (cdr list) 0 i) :compare compare))
	  (rank (subseq (cdr list) i) :compare compare))
    (list (cons (car list) (rank (cdr list) :compare compare)))))


;TODO numbering doesn't work.
(defun ranked-list (list &key number (number-upto (if number 4 1))
		              (via :header) replace-names
		              (index (list)) (header-add 2))
  "Makes a list with multiple levels for a list that went through RANK,
the elements must be: (level link-name object &optional first-obj)"
  (let ((via (if (listp via) via (list via)))
	(si -1)) ;Sub-list: TODO feel precarious..
    (flet ((rl (list &key (index index) (via via))
	     (ranked-list list :number-upto number-upto :via via
			  :replace-names replace-names
			  :index index :header-add header-add)))
      ;Single sub-elements not worth giving index.
      (unless (find-if-not (lambda (el) (listp(car el))) list)
	(return-from ranked-list 
	  (apply #'glist (cons :series (mapcar #'rl list)))))
      (apply #'glist
        (cons
	 (case (car via) (:header :series) (t via))
	 (collecting ()
	   (dolist (el list)
	     (collecting
	      (cond
		((listp (car el))
		 (setf- + si 1)
		 (rl el :index (cons si index)
		     :via (if-use (cdr via) via)))
;TODO allow sections to indicate that they don't want indexes.
		(t
		 (when index (setf- + (car index) 1))
		 (destructuring-bind
		       (level link-name object &optional first-obj) el
		   (declare (ignore first-obj))
		   (let*((obj ;Add number if numbering.
			  (if (<= level number-upto) 
			    (glist :series
				   (format nil "~{~a~^.~} " 
					   (reverse (copy-list index)))
				   object)
			    object))
			 (link-name (if-use ;Add link if link specified.
				     (assoc link-name replace-names
					    :test #'string=)
				     link-name))
			 (obj (if link-name
				  (link obj link-name) obj)))
		     (case (car via)
		       (:header
			(header (+ level header-add) obj))
		       (t
			obj))))))))))))))
  
(defun organize-contents (contents)
  "Organizes contents."
  (rank contents :compare (lambda (a b)
			    (<= (car a) (car b)))))

(defun use-contents
    (content-from &key number (number-upto (if number 4 2))
     (via :header) replace-names
     (index (list 0)))
  "Reads the (gil) files, organizing these hierarchically, in order to\
 produce a contents list."
  (unless (eql *lang* :contents)
    (ranked-list (organize-contents (gather-contents content-from))
		 :number number :number-upto number-upto :via via
		 :replace-names replace-names :index index)))
		      
(make-gil-definer :contents def-gil-method def-gil-method*)

(def-gil-method i-prep (thing t) () thing)
(def-gil-method i-glist (thing t) (list))
(def-gil-method i-note (thing t) (object))
(def-gil-method i-action (action t) (object))

(def-gil-method i-header (level t) (object))
(def-gil-method i-section (level t) (name object list)
  (push (list level name object (when *register-first* (car list)))
	*contents*))


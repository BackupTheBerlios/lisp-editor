
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
  
(defun organized-contents (contents)
  "Organizes contents."
  (unless *contents*
    (warn "Trying to use contents when no elements."))
  (rank contents :compare (lambda (a b)
			      (<= (car a) (car b)))))

;;Use data to make contents page.

;TODO it makes a mess of it..
(defun ranked-list (list &key number (number-upto (if number 4 0))
		    (include-upto 3)
		    (via :header) replace-names
		    (index (list)) (header-add 2) (header-nbsp-add 2)
		    top-level)
  "Makes a list with multiple levels for a list that went through RANK,
the elements must be: (level link-name object &optional first-obj)"
  (setf- reverse list)
  (let ((via (if (listp via) via (list via)))
	(si -1)) ;Sub-list: TODO feel precarious..
    (flet ((rl (list &key (index index) (via via))
	     (ranked-list list :include-upto include-upto
		:number-upto number-upto :via via
		:replace-names replace-names
		:index index
		:header-add header-add :header-nbsp-add header-nbsp-add))
	   (do-el (el)
	     (destructuring-bind
		   (level link-name title &optional first-obj) el
	       (declare (ignore first-obj))
	       (when (<= level include-upto)
		 (let*((obj ;Add number if numbering.
			(if (<= level number-upto) 
			  (glist :series
				 (format nil "~{~a~^.~} " 
					 (reverse (copy-list index)))
				 title)
			  title))
		       (link-name (if-use ;Add link if link specified.
				   (assoc link-name replace-names
					  :test #'string=)
				   link-name))
		       (title-obj
			(if link-name
			  (link link-name obj) obj)))
		   (case (car via)
		     (:header
		      (list
		       (apply #'header
			 `(,(+ level header-add) 
			    ,@(make-list (* header-nbsp-add (- level 1))
					 :initial-element :nbsp)
			    ,title-obj))))
		     (t
		      (list title-obj))))))))
      ;Single sub-elements not worth giving index.
      (unless (find-if-not (lambda (el) (listp(car el))) list)
	(return-from ranked-list 
	  (glist-list :series (mapcar #'rl list))))
      (glist-list (case (car via) (:header :series) (t (car via)))
	 (mapcan (lambda (el)
		   (cond
		     ((listp (car el)) ;TODO don't drag in first one..
		      (setf- + si 1)
		      (list(rl el :index (cons si index)
			       :via (if-use (cdr via) via))))
		     (t
		      (when index (setf- + (car index) 1))
		      (do-el el))))
		 list)))))

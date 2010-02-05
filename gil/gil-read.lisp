
(cl:in-package :cl-user)

(defpackage gil-read
  (:use :common-lisp :generic :denest)
  (:documentation "Stream reader for GIL.
TODO needs to filter out whitespace gil-execute")
  (:export gil-read execute gil-execute))

(in-package :gil-read)

(defun is-whitespace (ch)
  (case ch ((#\Newline #\Space #\Tab) t)))

(defun read-symbol
    (stream &key (buffer-len 256) (buffer (make-string buffer-len)))
  (declare (type string buffer))
  (do ((ch (read-char stream nil nil) (read-char stream nil nil))
       (i 0 (+ i 1)))
      ((or (not ch) (case ch
		      ((#\Newline #\Space #\Tab #\) #\( #\| #\{ #\} #\}
		        #\~ #\. #\,) t))
	   (>= i (length buffer)))
       (progn (unread-char ch stream)
	      (intern (string-upcase (subseq buffer 0 i)))))
    (setf (aref buffer i) ch)))

(defun read-integer
    (stream &key (buffer-len 256) (buffer (make-string buffer-len)))
  (declare (type string buffer))
  (do ((i 0 (+ i 1))
       (ch (read-char stream nil nil) (read-char stream nil nil)))
      ((or (not ch) (not (digit-char-p ch)))
       (progn (unread-char ch stream)
	      (values (parse-integer (subseq buffer 0 i)) i)))
    (setf (aref buffer i) ch)))

(defun read-number
    (stream &key (buffer-len 256) (buffer (make-string buffer-len)))
  (declare (type string buffer))
  (let ((x (read-integer stream :buffer buffer))
	(ch (read-char stream nil nil)))
    (cond 
      ((not ch)       x)
      ((char= #\. ch) (multiple-value-bind (y i)
			  (read-integer stream :buffer buffer)
			(+ x 0d0 (/ y (expt 10 i)))))
      (ch             (unread-char ch stream)
		      x))))

(defun gil-read
    (stream &key fn denest (depth 0) (*package* (find-package :gil-share))
                 (buffer-len 256) (buffer (make-string buffer-len)))
  "Reads 'gil files' which are basically lisp expressions with everything\
 defaultly strings ( and )  are not, \\ escapes, {} make use of denest,\
 $ makes cl:read take over for an expression, integers after whitespace\
 are read as integers.
For conveniant string/pathname/stream to stream conversion, use\
 generic:with-stream.

TODO optional, checked end-tag"
  (declare (type boolean denest)
	   (type fixnum depth buffer-len) (type string buffer))
  (denest
   (collecting (nil ret collect))
   (let ((i 0) prev-white))
   (macrolet ((col (item)
		`(progn (dump)
			(if fn (funcall fn ,item)
			       (collect ,item))))))
   (labels ((dump ()
	      (setq prev-white nil)
	      (unless (= i 0) ;Not empty or has whitespaces all over.
		(unless (when-let p
			    (position-if-not #'is-whitespace buffer)
			  (>= p i))
		  (collect (subseq buffer 0 i)))
		(setq i 0)))
	    (read-ch ()
	      (read-char stream nil nil))
	    (add-ch (ch)
	      (cond
		((< i (length buffer))
		 (setf (aref buffer i) ch
		       i (+ i 1)))
		(t
		 (dump)
		 (add-ch ch))))
	    (ret ()
	      (dump)
	      (return-from gil-read ret))))
   (do ((ch (read-ch) (read-ch)))
       ((not ch) (values))
     (case ch
       (#\( (col (cons (read-symbol stream :buffer buffer)
		       (gil-read stream :depth (+ depth 1)
				 :buffer buffer))))
       (#\) (if denest (error "Denested part not closed with an }")
		(ret))) ;Done with sublist.
       (#\$ (col (read stream))) ;Use normal CL reading.
       (#\# (when-let ch (read-ch) ;Read something as one object.
	      (unless
		  (when (char= ch #\()
		    (dump)
		    (dolist (el (gil-read stream
					  :depth depth :buffer buffer))
		      (col el))
		    t)
		(unread-char ch stream))))
    ;Denested - indicators
       (#\{ (col `(denest ,@(gil-read stream :denest t
			      :depth (+ depth 1) :buffer buffer))))
       (#\} (if denest (ret)
		(error "Non-denested closed with a denesting }.")))
       (#\\ (if-let ch (read-char stream nil nil) ;Escape character.
	      (add-ch ch)
	      (ret)))
       (t
	(if (is-whitespace ch)
	  (unless
	      (when-let ch (read-ch) ;Look out for numbers and #\$
		(unread-char ch stream)
		(cond
		  ((digit-char-p ch)
		   (col (read-number stream :buffer buffer))
		   t)
		  ((char= ch #\$)
		   t)))
          ;Don't read multiple whitespaces in sequence.
	    (unless prev-white
	      (add-ch ch)))
	  (add-ch ch))))
     (setq prev-white (is-whitespace ch)))))

(defun execute (input &key (fn #'eval))
  "Reads and then produces using whatever is in gil:*lang*. 
You need to funcall output if you're not using it in-line;
see gil-execute if you want to output."
  (if (listp input) (eval input)
    (with-open-file (stream input)
      (gil-read stream :fn fn))))

(defun gil-execute
    (from-file &key (to-file "doc.html") (to-path ".")
     (use-package '(:cl :gil :gils :gil-read))
     (funcall t))
  "Execute, but funcalls for you, and arguments set the special variables\
 accordingly."
  (let*((*default-pathname-defaults*
	 (pathname(format nil "~D~D"
		    *default-pathname-defaults* to-path)))
	(*standard-output*
	 (open to-file :direction :output
	       :if-does-not-exist :create :if-exists :supersede))
	(pkg-name (format nil "gil-file-~a" from-file)))
    (eval `(defpackage ,pkg-name (:use ,@use-package)))
    (execute from-file :fn
	     (lambda (item)
	       (let ((evalled (eval `(progn (in-package ,pkg-name)
					    ,item))))
		 (if funcall (funcall evalled) evalled))))
    (close *standard-output*)))

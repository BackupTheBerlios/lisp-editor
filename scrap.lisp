;  (let (escaped prev-escaped)
;    (loop for ch across line
;       do (case escaped
;	    (:string ;Excuses use of () at some cases, dont like code much.
;	     (case ch
;	       (#\" (setf escaped nil prev-escaped nil))
;	       (#\\ (setf escaped :single-escape))))
;	    (:single-escape
;	     (setf escaped prev-escaped))
;	    (:check-fence
;	     (case ch
;	       (#\| (setf escaped :temp-comment prev-escaped :temp-comment))
;	       (#\; (return)) ;Rest is comment.
;	       (#\( (setf- + cnt 1))
;	       (#\) (setf- - cnt 1))))
;	    (t
;	     (case ch
;	       (#\# (setf escaped :check-fence))
;	       (#\\ (setf escaped :single-escape prev-escaped nil))
;	       (#\" (setf escaped :string prev-escaped :string))
;	       (#\; (return)) ;Rest is comment.
;	       (#\( (setf- + cnt 1))
;	       (#\) (setf- - cnt 1)))))))
;  (let ((enter-line (make-string (length line)))
;	(j 0) escape)
;    (do ((i 0 (+ i 1)))
;	((>= (+ i 1) (length line)) nil)
;      (case escape
;	(:inline-comment
;	 (when (and (char= (aref line i) #\|)
;		    (char= (aref line (+ i 1)) #\#))
;	   (setf escape nil
;		 i (+ i 1))))
;	(t
;	 (case (aref line i)
;	   (#\" (case escape
;		  (:string (setf escape nil))
;		  (t       (setf escape :string))))
;	   (#\\
;	    (when (char= (aref line (+ i 1)) #\\)
;	      (setf (aref enter-line j) #\\
;		    j (+ j 1)))
;	    (setf- i + 1))
;	   (#\#
;	    (when (char= (aref line (+ i 1)) #\|)
;	      (setf escape :inline-comment
;		    i (+ i 1))))
;	   (#\(
;	    (unless escape (setf- + cnt 1)))
;	   (#\(
;	    (unless escape (setf- - cnt 1)))))))
;    

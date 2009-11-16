;TODO internal ones.

#|

 (defun concat-stringlist (list)
  (if (null (cdr list))
    (car list)
    (concatenate 'string (car list) " " (concat-stringlist (cdr list)))))

 (defun lml2-to-plaintext (code)
  "DOESN'T WORK PROPERLY"
  (denest
   (if (stringp  code) code)
   (destructuring-bind (name &rest body) code)
   (multiple-value-bind (namer args)
       (if (listp name) (values (car name) (cdr name)) name))
   (flet ((list-plaintext (list)
	    (mapcar #'lml2-to-plaintext body))
	 (plaintext (c)
	   (lml2-to-plaintext c))))
   (case namer
     (:p (format nil "~%~D~%"
		 (concat-stringlist (list-plaintext body))))
     (:a (list-plaintext body))
     ((:h1 :h2 :h3 :h4)
      (format nil "~D~%" (list-plaintext body))))))


 (defmethod autodoc (thing (way (eql :plain-string)))
  (with-slots (type name args) thing
    (values
     (format nil "~D ~D ~D~%~D~%"
	     type name (arguments-text args)
	     (documentation name (case type
				   ((defmacro defun) 'function)
				   ((defvar defparameter 'variable)))))
     (concatenate 'string
       (when (pslot thing :arg-docs)
	 (format nil "Documented arguments: ~{~a~^, ~}"
		 (collecting ()
		   (do ((i (pslot thing :arg-docs) (cddr i))) ((null i) nil)
		     (collecting (car i))))))))))

TODO you'll be better off doing it once you have coded more other stuff,
 like sorting of dependencies.
     (format nil "Function dependencies: ~{~a~, ~}" 

 (defun interactive-doc (thing &optional (in t) (out t))
  (format out (autodoc thing :plain-string))
  (format out "~%")
  (do ((read (read in) (read in)))
      ((or (string= read "q") (string= "quit") (string= "done")) nil)
    (cond
      ((string= read "list-dep")
       (format out "~D~%" (slot-value thing dep)))
      ((string= read "dep"

|#
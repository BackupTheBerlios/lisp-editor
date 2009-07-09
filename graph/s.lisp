

(macro-function 'tagbody)

(macroexpand-1 '(do ((i 0 (+ i 1))) ((>= i 5) nil) (print i)))

(require :asdf-install)

;(asdf:operate 'asdf:load-op "/home/jasper/oproj/cl-walker/")

(in-package :iterate)

(let ((*DEFAULT-PATHNAME-DEFAULTS*
       #p"/home/jasper/oproj/cl-walker/"))
  (asdf:operate 'asdf:load-op :cl-walker))

(with-open-file (stream #p"/home/jasper/oproj/cl-walker/cl-walker.asd")
  (iter 
    (let ((str (read-line stream nil nil)))
      (unless str (finish))
      (collect str))))
	

(asdf:operate 'asdf:load-op #p"/home/jasper/oproj/cl-walker")

(setf *debugger-hook* nil)

(let ((*debugger-hook* (lambda (condition 2nd)
			 (print (list condition 2nd)))))
  (+ 1 'a))

(+ 1 'a)


(defun one-of (choices &optional (prompt "Choice"))
   (let ((n (length choices)) (i))
     (do ((c choices (cdr c)) (i 1 (+ i 1)))
         ((null c))
       (format t "~&[~D] ~A~%" i (car c)))
     (do () ((typep i `(integer 1 ,n)))
       (format t "~&~A: " prompt)
       (setq i (read))
       (fresh-line))
     (nth (- i 1) choices)))

(defun my-debugger (condition me-or-my-encapsulation)
  (format t "~&Fooey: ~A" condition)
  (let ((restart (one-of (compute-restarts))))
    (if (not restart) (error "My debugger got an error."))
    (let ((*debugger-hook* me-or-my-encapsulation))
      (invoke-restart-interactively restart))))
 
 (let ((*debugger-hook* #'my-debugger))
   (+ 3 'a))


(ignore-errors ;Normally, this would suppress debugger entry
  (handler-bind ((error #'pprint)) ;But this forces debugger entry
    (error "Foo.")))

(require :ltk)
(require :iterate)

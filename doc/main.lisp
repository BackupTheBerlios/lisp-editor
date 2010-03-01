(cl:in-package :gil-user)

(require :autodoc)

(defun scan-stuff ()
  (let ((*default-pathname-defaults* 
	 #p"/home/jasper/proj/lisp-umac/"))
    (expr-scan:scan-file "generic.asd")
    (expr-scan:scan-file "denest.asd"))
  (let ((*default-pathname-defaults* 
	 #p"/home/jasper/proj/lisp-editor/gil/"))
    (expr-scan:scan-file "gil.asd"))
  (let ((*default-pathname-defaults* 
	 #p"/home/jasper/proj/lisp-editor/tools/"))
    (expr-scan:scan-file "expression-hook.asd")
    (expr-scan:scan-file "expression-scan.asd")
    (expr-scan:scan-file "autodoc.asd")))

(scan-stuff)

(defun package-link (pkg &rest objects)
  (apply #'autodoc-gil:mention `(defpackage ,pkg ,@objects)))

;;TODO currently it mixes the contents page.. Figure out how to give them
;; their respective contents side.
(let ((*lang* :html)
      (gils::*attempt-readable* nil)
      (gils:*handle-page* 
       (lambda (page)
	 (table
	  (list (table-el '(:colspan 2)
		  (inline-style "(font-size 250%)" "Lisp-editor")))
	  (list (table-el '(:valign :top)
			  (gil-info:use-contents :include-upto 1)
		  (gils::hr) "Hosted by" (newline)
;Note: currently not enough features to do it fully the same as:
;<a href="http://developer.berlios.de">
;<img src="http://developer.berlios.de/bslogo.php?group_id=0" width="124" height="32" border="0" alt="BerliOS Logo" /></a> 
		  (url-link "http://developer.berlios.de/"
		    (glist (mk file-image ;
			     :filename "http://developer.berlios.de/bslogo.php?group_id=0"))))
		page))))
      (autodoc-gil::*treat-args* nil)
      (autodoc-gil::*treat-title* :title-args))
  (run-gil
   `("../website.gil"
     ,@(mapcar (lambda (pkg)
		 (let ((gil-info::*contents* nil))
		   (autodoc-gil:document pkg :pkg :level 2)))
	       '(:generic :denest
		 :package-stuff :expression-hook :expression-scan
  	         :autodoc-gil ; TODO
		 :gil :gil-share :gil-style 
		 :gil-info :gil-read :gil-user
		 :gil-html)))
   "main.html"
   :to-path #p"/home/jasper/proj/lisp-editor/doc/html/"))

;;Doesn't work atm.. Because expression scan misses environment treatment, 
;; or just kinks?
(let ((*default-pathname-defaults* 
       #p"/home/jasper/proj/lisp-editor/doc/autodoc/")
      (ref
       (mapcar
	(lambda (path)
	  (when (stringp path)
	    (if (cl-fad:directory-pathname-p path)
	      (cons path 
		    (mapcan
		     (lambda (file &key (file-name (file-namestring file))
			      (len (length file-name))
			      (ext (if (> len 4) 
				    (subseq file-name (- len 4)) "")))
		       (when (or (string= ext ".asd")
				 (string= ext ".ASD"))
			 (list file-name)))
		     (cl-fad:list-directory path)))
	      path)))
	asdf:*central-registry*)))
  (dolist (r ref)
    (typecase r
      (string
       (let ((*default-pathname-defaults*
	      (pathname (directory-namestring r))))
	 (expr-scan:scan-file (file-namestring r))))
      (null)
      (list
       (let ((*default-pathname-defaults* (pathname (car r))))
	 (mapcar #'expr-scan:scan-file (cdr r)))))))

(let ((*default-pathname-defaults*
       #p"/home/jasper/oproj/lispbuilder-read-only/lispbuilder-sdl/"))
  (expr-scan:scan-file "lispbuilder-sdl.asd"))

(autodoc-gil:document :lispbuilder-sdl)

;;cl-fad, however does work.
(let ((*default-pathname-defaults*
       #p"/home/jasper/oproj/cl-fad-0.6.3/"))
  (expr-scan:scan-file "cl-fad.asd"))

(with-open-file (*standard-output* "doc/autodoc/cl-fad.html"
		 :direction :output :if-does-not-exist :create
		 :if-exists :supersede)
  (let ((gils::*section-page-level* 0)
	(*lang* :html))
    (call (autodoc-gil:document :cl-fad :pkg))))

(let ((*lang* :html))
  (call (glist (mk follow-link :name "lala") "meh" "kaka")))


(cl:in-package :gil-user)

(load "gil/tools/contents.lisp")
(require :more)
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

(defun side-paned-page (sidepane)
  (lambda (page)
    (table
     (list (table-el '(:colspan 2 :align :center)
		     (inline-style "font-size:250%" "Lisp-editor")))
     (list (table-el '(:valign :top :width "20%")
	     sidepane
	     (gils::hr) "Hosted by" (newline)
;Note: currently not enough features to do it fully the same as:
;<a href="http://developer.berlios.de">
;<img src="http://developer.berlios.de/bslogo.php?group_id=0" width="124" height="32" border="0" alt="BerliOS Logo" /></a> 
	     (url-link "http://developer.berlios.de/"
		       (glist (mk file-image ;
			 :filename "http://developer.berlios.de/bslogo.php?group_id=0"))))
	   (table-el '(:valign :top)
		     page))))) 

(defun mk-website ()
  "Makes the website. Only paginated stuff will appear as file, rest to\
 standard output."
  (let*((*default-pathname-defaults*
	 #p"/home/jasper/proj/lisp-editor/doc/html/")
	(gils::*attempt-readable* nil)
	(autodoc-gil::*treat-args* nil)
	(autodoc-gil::*treat-title* :title-args)
	(gil-info::*links* (make-hash-table))
	(site-contents
	 (gil-contents:use-contents
	  (gil-info:gather-contents  "../website.gil")
	  (gil-contents:c-el-seq
	   (:level-filter :to 1) :header :link)))
	(autodoc
	 (glist-list :series
	   (mapcar (lambda (pkg)
		     (autodoc-gil:document pkg :pkg :level 2))
		   '(:generic :denest
		     :package-stuff :expression-hook :expression-scan
		     :autodoc-gil
		     :gil :gil-share :gil-style 
		     :gil-info :gil-read :gil-user
		     :gil-html))))
	(autodoc-contents
	 (gil-contents:use-contents
	  (gil-info:gather-contents autodoc)
	  (gil-contents:c-el-seq
	   (:level-filter :to 1) :class-style :nbsp :link)))
	(*lang* :html))
    (let ((gils:*handle-page* (side-paned-page site-contents)))
      (call(execute "../website.gil")))
    (let ((gils:*handle-page* (side-paned-page
			       (glist :series site-contents
				 (inline-style "color:gray"
				  (header 4 (u(b "Autodoc:"))))
				 autodoc-contents))))
      (call autodoc))))

(mk-website)

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

;;documenting cl-fad, however does work.
(let ((*default-pathname-defaults*
       #p"/home/jasper/oproj/cl-fad-0.6.3/"))
  (expr-scan:scan-file "cl-fad.asd"))

(with-open-file (*standard-output* "doc/autodoc/cl-fad.txt"
		 :direction :output :if-does-not-exist :create
		 :if-exists :supersede)
  (let ((gils::*section-page-level* 0)
	(*lang* :txt))
    (call (autodoc-gil:document :cl-fad :pkg :level 2))))


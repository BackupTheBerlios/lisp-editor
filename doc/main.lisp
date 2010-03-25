(cl:in-package :cl-user)

(defpackage :lisp-ed-website
  (:use :common-lisp :gil :gil-vars :gil-share :gil-style
	:gil-autodoc :gil-read :gil-user))

(in-package :lisp-ed-website)

(setq expr-scan:*scan-result* (make-hash-table
 :test 'equalp))
(defun scan-stuff ()
  (let ((*default-pathname-defaults* 
	 #p"/home/jasper/proj/lisp-editor/libs/"))
    (expr-scan:scan-file "generic.asd")
    (expr-scan:scan-file "denest.asd")
    (expr-scan:scan-file "lisp-ed-package-stuff.asd")
    (expr-scan:scan-file "lisp-ed-path-stuff.asd"))
  (let ((*default-pathname-defaults* 
	 #p"/home/jasper/proj/lisp-editor/gil/"))
    (expr-scan:scan-file "gil.asd"))
  (let ((*default-pathname-defaults* 
	 #p"/home/jasper/proj/lisp-editor/tools/"))
    (expr-scan:scan-file "expression-hook.asd")
    (expr-scan:scan-file "expression-scan.asd")
    (expr-scan:scan-file "autodoc.asd")))

(scan-stuff)

(defun side-paned-page (sidepane)
  (gil-user::side-paned-page-handler
   :top-pane (inline-style "font-size:250%" "Lisp-editor")
   :top-pane-args '(:colspan 2 :align :center)
   :left-pane (series sidepane :hr
		      "Hosted by" :newline
;Note: currently not enough features to do it fully the same as:
;<a href="http://developer.berlios.de">
;<img src="http://developer.berlios.de/bslogo.php?group_id=0" width="124" height="32" border="0" alt="BerliOS Logo" /></a> 
		      (url-link "http://developer.berlios.de/"
			(glist (gen:mk file-image ;
			  :filename "http://developer.berlios.de/bslogo.php?group_id=0"))))))

;Note it has to be base-track, otherwise it is super of list, and it will 
; do :full with the list!
(def-document :full-with-hr*
    ((object expr-scan::base-track) &key)
  (series :hr (document :full object)))

(defun mk-website ()
  "Makes the website. Only paginated stuff will appear as file, rest to\
 standard output."
  (let*((*default-pathname-defaults* 
	 #p"/home/jasper/proj/lisp-editor/doc/src/")
	(*following-directory*
	 "../htdocs/")
	(*attempt-readable* nil)
	(gil-info::*links* (make-hash-table))
	(site-contents
	 (gil-contents:use-contents
	  (gil-info:gather-contents "website.gil")
	  (gil-contents:c-el-seq
	   (:level-filter :to 1) :header :link)))
	(autodoc
	 (glist-list :series
	   (mapcar
	    (lambda (pkg)
	      (document :pkg pkg
			:doc-manners '(:full-with-hr*)))
	    '(:generic :denest :alexandria
	      :package-stuff :expression-hook :expression-scan
	      
	      :gil :gil-vars :gil-share :gil-style
	      :gil-read :gil-info :gil-user
	      :gil-output-util :gil-html :gil-txt :gil-latex
	      
	      :gil-contents :gil-log
	      
	      :gil-autodoc))))
	(autodoc-contents
	 (gil-contents:use-contents
	  (let ((*following-directory* "../htdocs/autodoc/"))
	    (gil-info:gather-contents autodoc))
	  (gil-contents:c-el-seq
	   (:level-filter :to 1) :class-style :nbsp :link)))
	(*lang* :html))
    (with-open-file (stream "default.css" :direction :output
		     :if-exists :supersede :if-does-not-exist :create)
      ) ;;Clear .css.
    (let ((*handle-page* (side-paned-page site-contents)))
      (call(execute "website.gil")))
    (let ((*handle-page* (side-paned-page
			  (glist :series site-contents
				 (inline-style "color:gray"
				   (header 4 (u(b "Autodoc:"))))
				 autodoc-contents)))
	  (*following-directory* "../htdocs/autodoc/"))
      (call autodoc))))

(time (mk-website)) ;TODO it is warning me a bit.

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


;;documenting cl-fad, however does work.
(let ((*default-pathname-defaults*
       #p"/home/jasper/oproj/lispbuilder-read-only/lispbuilder-sdl/"))
  (expr-scan:scan-file "lispbuilder-sdl.asd"))

(let ((*default-pathname-defaults*
       #p"/home/jasper/proj/lisp-editor/doc/")
      (*lang* :html)
      (*attempt-readable* nil)
      (gil-html::*default-style-file* "default.css"))
  (with-open-file (*standard-output* "principles.html"
	 	     :direction :output :if-does-not-exist :create
		     :if-exists :supersede)
    (call (gil-html::style))
    (call (execute "principles.gil"))))

(let ((*lang* :latex))
  (call (section 4 "a" (link "miauw" "b") "kaka 24154 512" "35235252" (b "234"))))

(load "gil/output/latex.lisp")
	       

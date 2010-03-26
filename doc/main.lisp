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
(def-document :full-with-hr* ((object expr-scan::base-track) &key)
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
	   (mapcan
	    (lambda (pkg)
	      (when (expr-scan:access-result 'defpackage pkg)
		(list
		 (document :pkg pkg
			   :doc-manners '(:full-with-hr*)))))
	    '(:generic :denest :alexandria.0.dev
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

;;documenting cl-fad, however does work.
(let ((*default-pathname-defaults*
       #p"/home/jasper/oproj/lispbuilder-read-only/lispbuilder-sdl/"))
  (expr-scan:scan-file "lispbuilder-sdl.asd"))

	       

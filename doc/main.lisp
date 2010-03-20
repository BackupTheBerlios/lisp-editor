(cl:in-package :gil-user)

(require :more)
(require :autodoc)
(require :cl-dot)

(setq expr-scan:*scan-result* (make-hash-table :test 'equalp))
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
  (gil-autodoc:mention-obj (expr-scan:access-result 'defpackage pkg)
			   objects :start 1))

(defun mention+ (name &rest objects)
  "Mentions, assuming either function or variable and not ambiguous."
  (assert (not (keywordp name)))
  (if-let (mention (expr-scan:access-result
		    '(defun defgeneric defmacro defvar defparameter) name))
    (gil-autodoc:mention-obj mention objects)
    (u (string-downcase name))))

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
(gil-autodoc:def-document :full-with-hr*
    ((object expr-scan::base-track) &key)
  (series :hr (gil-autodoc:document :full object)))

(defun mk-website ()
  "Makes the website. Only paginated stuff will appear as file, rest to\
 standard output."
  (let*((*default-pathname-defaults*
	 #p"/home/jasper/proj/lisp-editor/doc/html/")
	(*attempt-readable* nil)
	(gil-info::*links* (make-hash-table))
	(gil-autodoc::*file-root-mention*
	 "/home/jasper/proj/lisp-editor/")
	(site-contents
	 (gil-contents:use-contents
	  (gil-info:gather-contents "../website.gil")
	  (gil-contents:c-el-seq
	   (:level-filter :to 1) :header :link)))
	(autodoc
	 (glist-list :series
	   (mapcar
	    (lambda (pkg)
	      (gil-autodoc:document :pkg pkg
				    :doc-manners '(:full-with-hr*)))
	    '(:generic :denest
	      :package-stuff :expression-hook :expression-scan
	      
	      :gil :gil-vars :gil-share :gil-style
	      :gil-read :gil-info :gil-user
	      :gil-output-util :gil-html :gil-txt :gil-latex
	      
	      :gil-contents :gil-log
	      
	      :gil-autodoc))))
	(autodoc-contents
	 (gil-contents:use-contents
	  (gil-info:gather-contents autodoc)
	  (gil-contents:c-el-seq
	   (:level-filter :to 1) :class-style :nbsp :link)))
	(*lang* :html))
    (with-open-file (stream "default.css" :direction :output
			    :if-exists :supersede :if-does-not-exist :create)
      (declare (ignore stream)))
    (let ((gil-vars:*handle-page* (side-paned-page site-contents)))
      (call(execute "../website.gil")))
    (let ((gil-vars:*handle-page* (side-paned-page
			  (glist :series site-contents
				 (inline-style "color:gray"
				   (header 4 (u(b "Autodoc:"))))
				 autodoc-contents))))
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

(let ((*default-pathname-defaults*
       #p"/home/jasper/oproj/lispbuilder-read-only/lispbuilder-sdl/"))
  (expr-scan:scan-file "lispbuilder-sdl.asd"))

;;documenting cl-fad, however does work.
(let ((*default-pathname-defaults*
       #p"/home/jasper/oproj/cl-fad-0.6.3/"))
  (expr-scan:scan-file "cl-fad.asd"))


(let ((*default-pathname-defaults*
       #p"/home/jasper/proj/lisp-editor/doc/autodoc/")
      (gils::*section-page-level* 0)
      (*lang* :txt))
  (with-open-file (*standard-output* "cl-fad.txt"
    	     	    :direction :output :if-does-not-exist :create
		    :if-exists :supersede)
    (call (gil-autodoc:document :pkg :cl-fad))))


(with-open-file (*standard-output* "doc/autodoc/cl-fad.tex"
		 :direction :output :if-does-not-exist :create
		 :if-exists :supersede)
  (let ((gils::*section-page-level* 0)
	(*lang* :latex))
    (call (gil-autodoc:document :pkg :cl-fad))))

(with-open-file (*standard-output* "doc/autodoc/cl-fad.html"
		 :direction :output :if-does-not-exist :create
		 :if-exists :supersede)
  (let ((gils::*section-page-level* 0)
	(*lang* :html))
    (call (gil-autodoc:document :cl-fad :pkg 2))))

(let ((*default-pathname-defaults*
       #p"/home/jasper/proj/lisp-editor/doc/")
      (*lang* :html)
      (gils::*attempt-readable* nil)
      (gil-html::*default-style-file* "default.css"))
  (with-open-file (*standard-output* "principles.html"
	 	     :direction :output :if-does-not-exist :create
		     :if-exists :supersede)
    (call (gil-html::style))
    (call (execute "principles.gil"))))


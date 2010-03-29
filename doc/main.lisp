(cl:in-package :cl-user)

(defpackage :lisp-ed-website
  (:use :common-lisp :alexandria :gil :gil-vars :gil-share :gil-style
	:gil-autodoc :gil-read :gil-user)
  (:export scan hard-scan make-asd make-website)
  (:documentation "Project website."))

(in-package :lisp-ed-website)

;(setq expr-scan:*scan-result* (make-hash-table :test 'equalp)
;      expr-scan:*package-list* nil)

(defun scan ()
  "Scan that goes via the systems.asd prefer it over hard-scan"
  (expr-scan:scan-file "systems.asd"))

;(scan)

(defun hard-scan ()
  "Hard scan that redoes the whole thing.
Disrecommended, if you want to add files, use expr-scan manually, or just\
 add them to systems.asd and do scan."
  (require :gtk)
  (require :cl-store)
  (require :cl-dot)
  (expr-scan:scan-file 
   (list "libs/" "gil/" "tools/" "doc/")
   :*load-first* t 
   :*judge* (lambda (file)
	      (case (intern (file-namestring file) :keyword)
		((:|clg.lisp| :|test.lisp|) t)
		(:|example.lisp| t)))))

;(hard-scan)

(defparameter *package-list*
  (mapcar
   #'expr-scan:name
   (expr-scan:list-packages-below-path "/home/jasper/proj/lisp-editor/"))
  "List of packages of this project")

(defun make-asd ()
  "Makes asd file for all the packages."
  (when (probe-file "systems.asd")
    (cl-fad:copy-file
     "systems.asd" 
     (format nil ".system-backups/systems~a.asd" (get-universal-time))))
  (asd-scanned:asd-scanned *package-list* 
     :to-path "/home/jasper/proj/lisp-editor/"))

;(make-asd)

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

(defun make-website ()
  "Makes the website. Only paginated stuff will appear as file, rest to\
 standard output."
  (let*((*default-pathname-defaults* 
	 #p"/home/jasper/proj/lisp-editor/doc/src/")
	(*following-directory*
	 "../htdocs/")
	(*path-root-mention* "/home/jasper/proj/lisp-editor/")
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
	    *package-list*)))
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

;(time (make-website)) ;TODO it is warning me a bit.

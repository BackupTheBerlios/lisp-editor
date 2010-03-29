(cl:in-package :cl-user)

(defpackage :scan-stuff
  (:use :cl :gil :gils :alexandria :gil-autodoc :package-stuff)
  (:export scan-libcl scan-self scan-cl-store autodoc asd )
  (:documentation "Attempt to scan many packages made by others, and to\
 then use that."))
.spring/
(in-package :scan-stuff)

(defvar *base-directory* "/home/jasper/oproj/")

(defun from-base (path)
  (pathname (concatenate 'string *base-directory* path)))

(defun scan-file
    (package &key load-first
     (rel-directory (format nil "~a/" (string-downcase package)))
     (file (format nil "~a.asd" (string-downcase package))))  
  "Scans a file."
  (let ((*default-pathname-defaults*
	 (pathname (format nil "~a~a" *default-pathname-defaults*
			   rel-directory))))
    (when load-first (load file))
    (expr-scan:scan-file file)))

;;Scan files.
(defun scan-self ()
  "Scan files of itself.

Note that there is a circularity here! The asd file is scanned for the\
 packages but is also created from that.

Add files by _manually_ scanning the new files and then doing asd."
  (expr-scan:scan-file "systems.asd"))

(defun scan-libcl
    (&optional (*default-pathname-defaults*
		(from-base "libcl-2009-10-27-beta/")))
  "Attempt to scan libcl packages. Note: it is a bit of work.

Scans a ton of files, a lot of non-successful stuff is commented.

Note that it loads them too; this may mean you have to watch out not to\
 rely on the packages without realizing it.(Always something to do)"
  (let ((expr-scan::*follow-asdf-systems*
	 (lambda (system)
	   (case (intern (format nil "~a" system) :keyword)
	     ((:anaphora-test :bordeaux-threads-test :cl-base64-tests) nil)
	     (t :also-load)))))
  (scan-file :alexandria)
  (scan-file :anaphora) ;Not the test. (Not sure it worked.)
  (scan-file :babel)
;Failed lift not found for test, 
  (scan-file :bordeaux-threads :load-first t) 
  (scan-file :cffi)
  (scan-file :chipz)
  (scan-file :chunga)
;:ch-image ch-util ;Failed
;:clem ;Failed, The value NIL
;is not of type
;  (OR (VECTOR CHARACTER) (VECTOR NIL) BASE-STRING PATHNAME
;      FILE-STREAM).
;   [Condition of type TYPE-ERROR]
  ;(scan-file :cl-base64) ;Failed?
  (scan-file :cl-cont :load-first t)
;:cl-containers ;Failed something about meta-moptilities, does asdf-load.
  (scan-file :cl-fad)
;:cl-graph ;failed.
  (scan-file :cl-jpeg)
;:cl-l10n ;Failed, asks for an execute.
;:cl-markdown  ;Failed, tries to get something from nonexistant directory.
;:cl-numlib  ;Failed 'erred in source file pointer'
  (scan-file :closer-mop)
;:closure-common ;Failed, error on some (function(lambda () form
;:cl-pdf ;Failed, similar problem.
;:cl-ppcre ;Failed idem
  (scan-file :cl-utilities)
  (scan-file :cl-vectors :load-first t)
;:cxml ;Failed, tries to get from cxml/package.lisp, it doesn't exist.
; Not sure how (require :cxml) subsequently does _not_ fail.
  (scan-file :defsystem-compatibility :load-first t)
  (scan-file :dynamic-classes :load-first t)
;:ffa ;Failed in pointer file.
;:flexichain Failed. same problem as :clem
;:flexi-streams Failed. Some lambda form
  (scan-file :fset)
  (scan-file :html-template)
;:ieee-floats Failed, couldnt find component fiveam.
  (scan-file :imago :load-first t)
 ;:ironclad Failed, some lambda form again...
;:iterate Failed, scan failed because reader doesn't do dispatch function.
;:kmrcl Failed, lambda form again
  (scan-file "libcl-compat" :file "split-sequence.asd")
  (scan-file :lml2 :load-first t)
;:local-time lambda form again.
  (scan-file :mel-base :load-first t)
;:metabang-bind Failed, seems to go for nonexistant
;    metabang-bind/bind-and-cl-ppcre/bind-cl-ppcre.lisp
;:metatilities Failed, similar, file:
;  metatilities/extensions/package-additional.lisp
;:metatilities-base similar;  metatilities-base/setup/package.lisp
  (scan-file :misc-extensions)
  (scan-file :moptilities :load-first t)
  (scan-file "parse-declarations" :file "parse-declarations-1.0.asd")
  (scan-file :png-read)
;:puri ;Failed, undefined function.
  (scan-file :salza2)
  (scan-file :skippy :load-first t)
;:spatial-trees Failed, tries to get spatial-trees/base/package.lisp
;:stefil Failed, scanner doesn't know dispatch functions.
;  (scan-file :tinaa :load-first t)
;:trees Failed, can't find truename of trees/rt.lisp
  (scan-file :trivial-features)
  (scan-file :trivial-gray-streams)
;:trivial-shell Failed, nonexistant trivial-shell/timeout/with-timeout.lisp
  (scan-file :vecto)
  (scan-file :zlib)
  (scan-file :zpb-ttf)
  (scan-file :zpng)))

(defun scan-cl-store (&optional (*default-pathname-defaults*
				 (from-base "cl-store/")))
  "Apparently successful scan of cl-store."
  (load "cl-store.asd")
  (expr-scan:scan-file "cl-store.asd"))

(def-document :object-doc
    ((object expr-scan::base-track) &key)
  (series
   :hr
   (section 2 (gil-autodoc::give-name object)
	    (gil-autodoc:document :title-with-args object)
    (gil-autodoc:document :description object)
    (p (gil-autodoc:document :dep object)))))

(defparameter *package-list* 
  '(:chunga :alexandria.0.dev :anaphora :babel :bordeaux-threads
    :cffi :chipz :chunga :cl-cont :cl-fad :cl-jpeg
    :closer-mop :cl-utilities :cl-vectors :defsystem-compatibility
    :dynamic-classes :fset :html-template :imago :split-sequence
    :lml2 :mel-base :misc-extensions :moptilities :parse-declarations
    :png-read :salza2 ;:skippy
    :tinaa 
    :trivial-features :trivial-gray-streams
    :vecto :zlib ;:zpb-ttf
    :zpng
    :cl-store)) ;The commented two got weird shit not recognized. 
; TODO see how/why

(def-document :my-package-list ((list list) &key)
  (section 1 "list" "List of various documented packages"
    (let ((gil-autodoc::*package-section-level* 3))
      (glist-list :series
	 (mapcan (lambda (el)
		   (when (expr-scan:access-result 'defpackage el)
		     (list(document :package-top (find-package el)
				    ))))
		 list)))))

(defun autodoc ()
  "Autodoc the stuff. 
TODO woefully wrong about the path-root-mention for :cl-store.
 change it such that it depends on the path listed in the scan result."
  (let*((*default-pathname-defaults* 
	 #p"/home/jasper/proj/lisp-editor/doc/src/")
	(*path-root-mention* (from-base "libcl-2009-10-27-beta/"))
	(*path-root-link* (from-base "libcl-2009-10-27-beta/"))
	(gil-vars:*following-directory* "../htdocs/autodoc/")
	(gils::*section-page-level* 1)
	(gil-info::*links* (make-hash-table))
	(autodoc-list
	 (mapcar 
	  (lambda (pkg)
	    (document :pkg pkg :doc-manners '(:object-doc)))
	  *package-list*))
	(autodoc (glist-list
		  :series (cons (document :my-package-list *package-list*)
				autodoc-list))))
    (mapcar (lambda (*lang*)
	      (call autodoc))
	    '(:info :html))))

(defun asd-outside ()
  "Make a .asd for the package.
TODO same as autodoc."
  (let ((asd-scanned:*system-name-hook* #'package-keyword))
    (asd-scanned:asd-scanned *package-list* 
       :to-file "other-libs.asd"
       :to-path "/home/jasper/oproj/libcl-2009-10-27-beta/")))

(defparameter *package-results*
  (mapcar (lambda (pkg)
	    (expr-scan:access-result 
	     'defpackage (graph-scanned::package-keyword pkg)))
	  *package-list*)
  "Lists the results corresponding to *package-list*")

(defun graph-package (pkg &key with-package-top)
  "Make a graph of the functions in package."
  (let ((package-result
	 (expr-scan:access-result 
	  'defpackage (graph-scanned::package-keyword pkg))))
    (cl-dot:dot-graph
     (cl-dot:generate-graph-from-roots
      'graph-scanned::full
      (if with-package-top
	(list package-result)
	(remove-if (lambda (res)
		     (eql (type-of res) 'expr-scan:track-package))
		   (cl-dot:graph-object-points-to
		    'graph-scanned::full package-result))))
     (format nil "doc/htdocs/autodoc/data/~a.png" pkg)
     :format :png)))

(defun graph ()
  "Graph the functions of the packages."
  (mapcar #'graph-package *package-list*))

(defun graph-package-wise ()
  "Graph in the sense of the packages."
  (let*((root *package-results*)
	(dgraph
	 (cl-dot:generate-graph-from-roots 'package *package-results*)))
    (cl-dot:dot-graph dgraph "doc/htdocs/autodoc/data/packages.png"
		      :format :png)))

;;Having this docced would be nice..
;(let ((*default-pathname-defaults*
;       #p"/home/jasper/oproj/clg-0.93/gtk/"))
;  (expr-scan:scan-file "gtk.asd"))

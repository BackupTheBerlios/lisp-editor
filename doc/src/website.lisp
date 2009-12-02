
(require :iterate)
(load "generic.lisp")

(require :lml2)
(load "mk-doc/mk-website.lisp")

(in-package :mk-website)

(defvar *file-links* t)

(defun idem-href (name &key (h3 :h3))
  `((:a :href ,(format nil "Main.html#~D" name)) (,h3 ,name)))

(defun idem-name (name &key (h3 :h3))
  `((:a :name ,name) (,h3 ,name)))

(defparameter *autodoc-list*
  (iter
    (for p in '(:generic :lisp-ed :down-graph :down-graph-draw
		:autodoc :mk-website))
    (appending
     `((:h4 ,(symbol-name p))
       (:p ((:a :href ,(format nil "autodoc/~D/Documentation.html"
			       (string-downcase(symbol-name p))))
	    "Single-page"))
       (:p ((:a :href ,(format nil "autodoc/~D/Contents.html"
			       (string-downcase(symbol-name p))))
	    "List of pages"))))))

(defparameter *page-list*
  `(("doc/Main"
     (:head
      (:title "Main page of lisp-ed.")
      ((:meta :http-equiv "Content-Type"
	      :content "text/html; charset=iso-8859-1"))
      ((:meta :name "description"
	      :content "Main page of lisp-ed.")))
     (:p "Lisp-ed is a project to make a lisp editor, with a lot of\
 tools on it. There is still a lot to be done.(The TODOs are not exhaustive.)
I also hope to share/combine work with "
	 ((:a :href "http://phil.nullable.eu/")"Able") ".")
     ,(idem-name "Lisp-ed") ;Lisp-ed..
     (:p "Currently able to use both regular and emacs-like\
 copy-pasting and such. The function that starts the editor has an\
 argument that is a function that makes various bindings. The definition of this
 function, however reads like a list. This way, one can just compose \
functions to set ones the preferences.")
     (:p "Of course, code can be executed right from the buffer.")
     (:p "TODO"
	 (:ul
	  (:li "Better customizability of the buffers.")
	  (:li "Syntax highlighting.")
	  (:li "(Optional)Noobie stuff, for inefficient mouse driven code.")
	  (:li "The things relating to stuff below.")))
     ,(idem-name "Code-scan") ;Code-scan.
     (:p "Used to get allsorts of information out of the code.\
 Used to get links between functions and such. "
	 ((:a :href "code-scan-workings.html")"Here is how it works."))
     (:p "TODO"
	 (:ul
	  (:li "More symmetric handling of different macros, defun and\
 defmacro needen't be special.")
	  (:li "Problem with trying to scan code-scan itself. (Presumably\
 code-scan doesn't affect the result; it doesn't change the macrohooks'\
 return.")
	  (:li "Getting info about variables too")
	  (:li "Putting information about links between functions/macros on\
 the function/macro trackers.")))
     ,(idem-name "Autodoc") ;Autodoc.
     (:p "Automatic documentation, that uses "
	 ((:a :href "http://www.cliki.net/lml2")"LML2")
" and documentation from code-scan.\
 Uses mk-website, and can do both all-on one page, and separate pages.")
     (:p "TODO"
	 (:ul
	  (:li "Use information like whether symbols are external or not,
sort pages by whether they're external.")
	  (:li "Creation of autodocumentation is slow and doesn't scale\
 well. What can be done to improve?")
	  (:li "Something better then LML2? I am currently using it via\
 eval.")
	  (:li "Better arguments representation.")
	  (:li "Variables, arbitrary macros, when specified how to\
 represent them.")
	  (:li "Color coding different things, like different colors for \
macros, functions variables etcetera. (CSS?)")
	  (:li "Different fore and background color; white on black,\
 etcetera.")))
     (:h3 ((:a :name "get-it")"Getting lisp-ed"))
     "Lisp-ed can be obtained via "
     ((:a :href "http://developer.berlios.de/git/?group_id=10956")"Git")
     " or "
     ((:a :href "http://download.berlios.de/gitdumps/lisp-editor-git.tar.gz")
      "Berlios' dayly tarball of git")
     
     ,(idem-name "Mk-website") ;Mk-website.
     (:p "Very simple website making tool.
TODO is automatic linking; must keep working regardless of single page or\
 multiple pages.")
     (:h3 "Autodocumentation for this project")
     ,@*autodoc-list*)
    ("doc/code-scan-workings"
     (:h3 "How codescan works")
     "It works by calling the function that does the macroexpanding. Then\
 it calls the scanning function on the result:"
     (:ul
      (:li "When it finds a s-expression for which (macro-function symbol)\
 produces non-nil, for those it stops; the macroexpansion hook will pick it\
 up.")
      (:li "Looks for special-operator-p stuff, for a bunch of these, how\
 the scanning continues is determined manually. If it isn't specified\
 manually, it stops the scan.")
      (:li "When not fboundp, it stops. This is still a problem for flet's,\
 i don't know if it is for macrolet. (macroexpand '(macrolet ...)) doesn't\
 seem to do anything, maybe macrolet can be replaced with something that\
 immediately expands all of it's elements. Flets are still a problem."))
     (:p "Some packages are ignored, like SBCL internal stuff: :sb-impl,\
 :sb-int, :sb-c, :sb-pcl, :sb-kernel.")
     (:p "Of course the scanning is to gather information. It has defvars\
 to store the current function(or macro) in-fun, and it stores function\
 usage in functions, links between functions in function-links, currently\
 defmacro and defun are treated differently, and there is some undertested\
 (probably doesn't work) code to more generally store information. I am\
 planning to just have a function that is called at every level to treat\
 everything more uniformly.(defun, defmacro no longer being special.) That\
 way, autodoc(also in lisp-ed project), can also document macros made by\
 the user, simply by providing a scanner and a documenter for the macro."
	))))

(let ((logo
       '((:a :href "http://developer.berlios.de")
	 ((:img :src "http://developer.berlios.de/bslogo.php?group_id=0"
	   :width 124 :height 32 :border 0 :alt "BerliOS Logo")))))
  (mk-website
   :contents
   `((:ul
      (:li (:p ,(idem-href "Lisp-ed")))
      (:li ,(idem-href "Code-scan")
	   (:p ((:a :href "code-scan-workings.html") "And how it works.")))
      (:li ,(idem-href "Autodoc"))
      (:li ,(idem-href "Mk-website"))
      (:li
       ((:a :href "#get-it") "Getting lisp-ed")
       (:ul
	(:li ((:a :href "http://developer.berlios.de/git/?group_id=10956")
	      "With Git"))
	(:li ((:a :href "http://download.berlios.de/gitdumps/lisp-editor-git.tar.gz")
	      "Berlios' dayly tarball of git")))))
     (:hr)
     (:h4 "Autodocumentation:")
     ,@*autodoc-list*)
   :page-list *page-list*
   :top `((:h1 "Lisp-ed"))
   :bottom `(,logo)))
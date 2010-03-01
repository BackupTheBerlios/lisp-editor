
(cl:in-package :cl-user)

(defpackage :doc-lisp-ed
  (:use :cl :denest :gil :gil-share)
  (:export doc))

(in-package :doc-lisp-ed)

(defun doc (&key lang to-file (to-path "doc/")
	    (*lang* lang)
	    (*default-pathname-defaults*
	     (pathname(format nil "~D~D" *default-pathname-defaults* to-path)))
	    (*standard-output* (if to-file (open to-file :direction :output
						 :if-does-not-exist :create :if-exists :supersede)
				   *standard-output*)))
  (let*((*cur-page* (make-instance 'gils::page))
	*pages*
	(res
  (funcall
   (glist :series
     (section 1 "Intro" "Intro"
       (section 2 "GIL" "General Interface Library/Language"
	 (p "I was annoyed that autodoc(see below) could only output in one language. "
	    "I also simply felt that we needn't have separation between markup, web, and gui, and that "
	    "Code=data should be 'Code as manifested in function usuage'=data.")
	 (p "As such, i made/am making a library that allows one to specify documents/guiworks just with methods and functions.(Macros might be useful\
 enough to include, but i'll mostly avoid them.)  The methods are mostly distinguished for purposes of indicating meaning."
	    "Objects inputted determine the actual content. For instance, strings represent text. "
	    "See " (link "at 'GIL'" "GIL") " for more.")) ;TODO found difficulty.. Need to know whether page/link -name, shouldn't have to.
       "Lisp-ed was originally a project to make a new lisp-editor. However, i didn't work very "
       "project-focussed and made a bunch of other stuff, making the aim more about gui/markup "
       "and autodocumentation."
       (section 2 "Scanning code" "Scanning code"
	 "Scanning of the code. It is in two separate parts:"
	 (alt-point-list
	  "An implementation of an expression-hook, it effectively reimplements CLs macroexpansion for this."
	  "A scanner that collects information based on that. As it works with an expression hook, and macro"
	  "/function called in any way can be automatically documented, if instructed how to document it."))
       
       (section 2 "Autodocumentation" "Autodocumentation"
	 "Based on the scan data, so can make documentation from any expression useage. "
	 "Being annoyed by LML2, and being able to output only html, i made GIL."))
     
     (section 1 "GIL" "General Interface Library/Language (Explanation)"
       "GIL uses methods to define the process of producing markup/html/gui. The methods in the :gil packagage "
       "are named by meaning, and the :gil-share suggests some implementation, in the hope that it can help make "
       "customizing output more uniform. As GIL is meant to be very general and output might not be, the output "
       "may ignore some things. For instance plain-text output will ignore links whereas HTML will not."
       
       (section 2 "The methods" "The methods"
	 "The methods by convention have as first argument the language, and start with 'i-', objects they is/are "
	 "The last arguments.(TODO not actually so yet.) There are 'wrapping functions' which defers the language "
	 "argument to the special variable *lang* and give some &rest, and other such conveniences."
	 "One should not use i-... when you " (i "use") " GIL, see " (link "Using gil" "GIL-use")".")

       (p "Using methods means you can easily extend it by making methods for your own symbols. You can also make "
	  "'dud-classes' and derive from them. You can also use them as functions, because they " (i "are")" functions.")
       
       (section 3 "i-prep" "i-prep (lang object)"
	 "Prepares an object for use.")
       
       (section 3 "i-glist" "i-glist (lang way list)"
	 "This is meant to treat lists. Where 'way' is the manner of treatment. For instance, :p is for making one "
	 "paragraph, and listing things for it :series is just a series of things without any idea what it is. "
	 ":point-list, :alt-point-list, :numbered-list for listing with dots, numbered."
	 ":gil-share has the dot-list class which can do more styles.")
       
       (section 3 "i-note" "i-note (lang note object)"
	 "Adds a note to the action to the object, for instance link-end points.(class link in note) Or :bold, "
	 ":italic, and :underlined. (b,i,u in gil-share) So very general, like that."
	 "(Perhaps there should be a comment-note)")
       
       (section 3 "i-action" "i-action (lang action object)"
	 "Adds an action to an object. For instance linking is considered an action. Note that many "
	 "interactive objects will have actions as implied by their type of object, but if any are added "
	 "not-as-objects, separately, it should be done through action.")
       
       (section 3 "i-header" "i-header (lang level object)"
	 "Title for a subsection. Level indicates the significance, which goes across the whole sprectrum, from "
	 "the title of the whole thing to a mere subsection. "
	 "Currently significance in integers>=0 with 0 most significant.")
       
       (section 3 "i-section" "i-section (lang level name object paragraphs)"
	 "Defaultly, it just produces a paragraph with a header in front. However, this can(and is, for :html) "
	 "be used for other distinctions too, like noting the names to work like link endpoints, and splitting "
	 "the text up into separate pages/files.")
       
       "See the autodocumentation(TODO) for more.")
     
     (section 1 "GIL-use" "Using GIL"
       "TODO, note it is pretty new atm and not exactly stable. If things get to nested, try "
       (link "the denest macro" "http://lisp-umac.berlios.de/main.html") ", which removes nestedness."
       (section 2 "Notation" "Notation"
	 "The basic lisp notation is less efficient one would want. Particularly the \" everywhere is a little "
	 "cumbersome. I will make a parse that reads s-expressions a little different, that will read everything"
	 " as string immediately, unless (x with x an alphanumeric, which make function calls, and $x takes x as"
	 " a variable, or when x '(', reads it as the lisp-reader would. I might also add something to indicate a denest."))
     ))))
  (when to-file
    (close *standard-output*))
  (values res *pages*)))
    
(doc :lang :html :to-file #p"doc.html")


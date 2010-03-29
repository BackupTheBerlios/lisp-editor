;;Autogenerated asd file by asd-scanned.

(cl:in-package :cl-user)

(use-package '(:asdf))

(defsystem :graph-scanned
  :description "Uses cl-dot to graph the objects.
NOTE still in development, some 'playing' in it.
TODO: 
* Determine if connections are to be expected depending on context.
* work it into the autodoc"
  :serial t
  :depends-on (:alexandria :package-stuff :cl-dot :expression-scan :gil-autodoc :alexandria)
  :components (
  (:module "tools" :components (
    (:file "graph-scanned")))))

(defsystem :gil-store-log
  :description ""
  :serial t
  :depends-on (:alexandria :generic :denest :cl-store :store-log :path-stuff :gil :gil-share :gil-vars :gil-comms :gil-read :gil-output-util)
  :components (
  (:module "tools" :components (
    (:file "gil-store-log")))))

(defsystem :expression-scan
  :description "Can create and use expression-hook to obtain information about code.
Any s-expression can be tracked. (So macros and functions can be tracked.)
"
  :serial t
  :depends-on (:alexandria :generic :denest :package-stuff :path-stuff :expression-hook :cl-fad :alexandria)
  :components (
  (:module "tools" :components (
    (:file "expression-scan")))))

(defsystem :expression-hook
  :description "Macroexpands code purely by itself. *expression-hook* continues it,
 and must call further expand-hook.
Used for gathering information on code autodoc via expression-scan."
  :serial t
  :depends-on (:alexandria :denest :package-stuff :alexandria)
  :components (
  (:module "tools" :components (
    (:file "expression-hook")))))

(defsystem :gil-autodoc
  :description "Produces GIL 'code' documentation.
Note: keyword 'way' arguments are the defaults.

TODO messy file."
  :serial t
  :depends-on (:alexandria :generic :denest :package-stuff :path-stuff :gil :gil-share :expression-scan :alexandria)
  :components (
  (:module "tools" :components (
    (:file "autodoc")))))

(defsystem :asd-scanned
  :description "Automatically make .asd files from scan results and given packages
 to do so on.

WARNING TODO currently only makes :depends-on if it is :used ! "
  :serial t
  :depends-on (:alexandria :generic :denest :expression-scan :package-stuff :path-stuff :alexandria)
  :components (
  (:module "tools" :components (
    (:file "asd-scanned")))))

(defsystem :gil-create-util
  :description "For creating stuff."
  :serial t
  :depends-on (:gil :gil-share)
  :components (
  (:module "gil" :components (
    (:file "util")))))

(defsystem :gil-user
  :description "Package for the user of gil.."
  :serial t
  :depends-on (:alexandria :denest :gil :gil-vars :gil-share :gil-style :gil-read :gil-info)
  :components (
  (:module "gil" :components (
    (:file "user")))))

(defsystem :gil-log
  :description "Extends log system for executing gil files and
 producing RSS.

Sortah depricated before i really started using it ><"
  :serial t
  :depends-on (:alexandria :generic :log :path-stuff :gil :gil-read :gil-output-util :gil-comms :cl-fad :gil-html :gil-share :alexandria)
  :components (
  (:module "gil" :components (
    (:module "tools" :components (
      (:file "log")))))))

(defsystem :gil-index
  :description "Uses indexed declared notable words, makes a page
 linking back to them."
  :serial t
  :depends-on (:gil :gil-share :gil-contents)
  :components (
  (:module "gil" :components (
    (:module "tools" :components (
      (:file "index")))))))

(defsystem :gil-contents
  :description "Tool to make contents from information contained in 
gil-info::*contents* after gil-executed with *lang* is :info.

Function gather-contents will do the scan :info does and provide input
 for use-contents. c-el is various methods that give you ways to treat
 the elements of the contents."
  :serial t
  :depends-on (:gil :gil-share :gil-style)
  :components (
  (:module "gil" :components (
    (:module "tools" :components (
      (:file "contents")))))))

(defsystem :gil-vars
  :description "Various variables determining how things are done."
  :serial t
  :depends-on (:gil)
  :components (
  (:module "gil" :components (
    (:module "specify" :components (
      (:file "vars")))))))

(defsystem :gil-style
  :description "Style system, using CSS. (In future might want to
 expand on that."
  :serial t
  :depends-on (:generic :gil)
  :components (
  (:module "gil" :components (
    (:module "specify" :components (
      (:file "style")))))))

(defsystem :gil-share
  :description "Various specifying objects and directly attached
 functions to help use them."
  :serial t
  :depends-on (:alexandria :gil :gil-vars :generic :alexandria)
  :components (
  (:module "gil" :components (
    (:module "specify" :components (
      (:file "share")))))))

(defsystem :gil
  :description "GIL: General Interface _Library_ (But be sure to confuse people with 
the L being Language ;) )

Note that this package does not implement any implementation! 
It only defines functions for it.
Note that you will want to use DENEST with this!

A lot of the defvars are some things implementors can hang on to,
so they're applicable to multiple implementations."
  :serial t
  :depends-on (:alexandria :alexandria)
  :components (
  (:module "gil" :components (
    (:module "specify" :components (
      (:file "gil")))))))

(defsystem :gil-comms
  :description "Various variables communicating from :info and/or
 such. Not at the user end."
  :serial t
  :depends-on (:gil)
  :components (
  (:module "gil" :components (
    (:module "specify" :components (
      (:file "comms")))))))

(defsystem :gil-read
  :description "Stream reader for GIL.
TODO needs to filter out whitespace gil-execute"
  :serial t
  :depends-on (:alexandria :denest :path-stuff :gil :generic :alexandria)
  :components (
  (:module "gil" :components (
    (:file "read")))))

(defsystem :gil-output-util
  :description "Some basic utility functions for manipulating strings,
 writing."
  :serial t
  :depends-on (:generic :gil :gil-share :gil-vars :gil-comms)
  :components (
  (:module "gil" :components (
    (:module "output" :components (
      (:file "util")))))))

(defsystem :gil-txt
  :description "Text output of General Interface Library/Language.
Goes by symbol :txt

TODO not tested very deeply."
  :serial t
  :depends-on (:gil-output-util :gil :gil-vars :gil-share :gil-style)
  :components (
  (:module "gil" :components (
    (:module "output" :components (
      (:file "txt")))))))

(defsystem :gil-latex
  :description "Latex output of General Interface Library/Language.
Goes by symbol :latex

TODO no styles, latex aught to be able to do a bunch of it.

TODO/NOTE completely untested other then inspection of result.

TODO equations would be neat to have, and crazy not to have.
1)I want the capability to have identical interface in other outputs!
2)Via s-expressions first, non-s-expression input form later."
  :serial t
  :depends-on (:denest :gil-output-util :gil :gil-vars :gil-share :gil-style)
  :components (
  (:module "gil" :components (
    (:module "output" :components (
      (:file "latex")))))))

(defsystem :gil-html
  :description "Gil->html, not that the files linked internally are all 
tracked by gil-info."
  :serial t
  :depends-on (:alexandria :generic :denest :path-stuff :gil-output-util :gil :gil-share :gil-style :gil-vars :gil-comms :gil-info :alexandria)
  :components (
  (:module "gil" :components (
    (:module "output" :components (
      (:file "html")))))))

(defsystem :gil-info
  :description "Contains gathering data for contents, and creation of contents section.
Gathering contents also registers and handles links if not already
 registered."
  :serial t
  :depends-on (:alexandria :denest :gil :gil-vars :gil-comms :gil-share :gil-read)
  :components (
  (:module "gil" :components (
    (:file "info")))))

(defsystem :store-log-test
  :serial t
  :depends-on (:generic :cl-fad :store-log)
  :components (
  (:module "libs" :components (
    (:module "test" :components (
      (:file "store-log")))))))

(defsystem :path-stuff-test
  :serial t
  :depends-on (:generic :path-stuff)
  :components (
  (:module "libs" :components (
    (:module "test" :components (
      (:file "path-stuff")))))))

(defsystem :log-test
  :serial t
  :depends-on (:generic :cl-fad :log)
  :components (
  (:module "libs" :components (
    (:module "test" :components (
      (:file "log")))))))

(defsystem :store-log
  :description "Uses cl-store to keep track of files from a single
 file, denoting when they're encountered/first added.

Use cl-store's store to write and read-log to read the log. (both
 extendable with methods.)
Use add-entry-hook to change the entry-type to something else than
 base-entry.

With-log will read and later write for you."
  :serial t
  :depends-on (:alexandria :cl-store :path-stuff :alexandria :cl-fad)
  :components (
  (:module "libs" :components (
    (:file "store-log")))))

(defsystem :path-stuff
  :description "Some stuff to assist with paths and paths."
  :serial t
  :depends-on (:alexandria :alexandria)
  :components (
  (:module "libs" :components (
    (:file "path-stuff")))))

(defsystem :package-stuff
  :description "Some stuff to do with packages."
  :serial t
  :depends-on (:alexandria :alexandria)
  :components (
  (:module "libs" :components (
    (:file "package-stuff")))))

(defsystem :log
  :description "Logs files, when they change, allows to store.
Write-log writes the all of *log-file*"
  :serial t
  :depends-on (:alexandria :alexandria :cl-fad)
  :components (
  (:module "libs" :components (
    (:file "log")))))

(defsystem :generic
  :description "Assortment of little useful macros/functions."
  :serial t
  :depends-on (:alexandria :alexandria)
  :components (
  (:module "libs" :components (
    (:file "generic")))))

(defsystem :denest
  :description "Macro to denest, remove nestedness of macros
/functions. Was somewhat of a relevation to me and suprising that this
 function isn't being screamed from the rooftops. Turns out that macros
 like iterate might just be trying to fight nestedness.(Mostly)

A few macros are given, some are on keywords and specific to denest, to 
 save namespace.  They do _exactly_ the same as regular macros however! You
 can use them with the USE-DENEST macro. The macros supplied in this packageIf
with non-keyword symbols are exported.

TODO remove some nastyness that might happen if denest is nested with self..
Can it be done?"
  :serial t
  :depends-on ()
  :components (
  (:module "libs" :components (
    (:file "denest")))))

(defsystem :lisp-ed-website
  :description "Project website."
  :serial t
  :depends-on (:alexandria :gil :gil-vars :gil-share :gil-style :gil-autodoc :gil-read :gil-user :gil-info :gil-contents :generic :asd-scanned :cl-fad :expression-scan)
  :components (
  (:module "doc" :components (
    (:file "main")))))


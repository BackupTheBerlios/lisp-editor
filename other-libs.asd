;;Autogenerated asd file by asd-scanned.

(cl:in-package :cl-user)

(use-package '(:asdf))

(defsystem :chunga
  :serial t
  :depends-on (:common-lisp :trivial-gray-streams)
  :components (
  (:module "chunga"
    (:file "specials.lisp")
    (:file "util.lisp")
    (:file "known-words.lisp")
    (:file "conditions.lisp")
    (:file "read.lisp")
    (:file "streams.lisp")
    (:file "input.lisp")
    (:file "output.lisp"))))

(defsystem :alexandria.0.dev
  :serial t
  :depends-on (:common-lisp)
  :components ())

(defsystem :anaphora
  :description "ANAPHORA provides a full complement of anaphoric macros. Subsets of the
functionality provided by this package are exported from ANAPHORA-BASIC and
ANAPHORA-SYMBOL."
  :serial t
  :depends-on (:common-lisp)
  :components (
  (:module "anaphora"
    (:file "early.lisp")
    (:file "symbolic.lisp")
    (:file "anaphora.lisp"))))

(defsystem :babel
  :serial t
  :depends-on (:common-lisp :babel-encodings :alexandria.0.dev)
  :components ())

(defsystem :bordeaux-threads
  :description "BORDEAUX-THREADS is a proposed standard for a minimal
  MP/threading interface. It is similar to the CLIM-SYS threading and
  lock support, but for the following broad differences:

  1) Some behaviours are defined in additional detail: attention has
     been given to special variable interaction, whether and when
     cleanup forms are run. Some behaviours are defined in less
     detail: an implementation that does not support multiple
     threads is not required to use a new list (nil) for a lock, for
     example.

  2) Many functions which would be difficult, dangerous or inefficient
     to provide on some implementations have been removed. Chiefly
     these are functions such as thread-wait which expect for
     efficiency that the thread scheduler is written in Lisp and
     'hookable', which can't sensibly be done if the scheduler is
     external to the Lisp image, or the system has more than one CPU.

  3) Unbalanced ACQUIRE-LOCK and RELEASE-LOCK functions have been
     added.

  4) Posix-style condition variables have been added, as it's not
     otherwise possible to implement them correctly using the other
     operations that are specified.

  Threads may be implemented using whatever applicable techniques are
  provided by the operating system: user-space scheduling,
  kernel-based LWPs or anything else that does the job.

  Some parts of this specification can also be implemented in a Lisp
  that does not support multiple threads. Thread creation and some
  thread inspection operations will not work, but the locking
  functions are still present (though they may do nothing) so that
  thread-safe code can be compiled on both multithread and
  single-thread implementations without need of conditionals.

  To avoid conflict with existing MP/threading interfaces in
  implementations, these symbols live in the BORDEAUX-THREADS package.
  Implementations and/or users may also make them visible or exported
  in other more traditionally named packages."
  :serial t
  :depends-on (:common-lisp)
  :components ())

(defsystem :cffi
  :serial t
  :depends-on (:common-lisp :cffi-sys :babel-encodings)
  :components ())

(defsystem :chunga
  :serial t
  :depends-on (:common-lisp :trivial-gray-streams)
  :components (
  (:module "chunga"
    (:file "specials.lisp")
    (:file "util.lisp")
    (:file "known-words.lisp")
    (:file "conditions.lisp")
    (:file "read.lisp")
    (:file "streams.lisp")
    (:file "input.lisp")
    (:file "output.lisp"))))

(defsystem :cl-cont
  :description "A library that implements continuations by transforming Common
  Lisp code to continuation passing style."
  :serial t
  :depends-on (:common-lisp)
  :components ())

(defsystem :cl-fad
  :serial t
  :depends-on (:common-lisp)
  :components (
  (:module "cl-fad"
    (:file "fad.lisp"))))

(defsystem :closer-mop
  :serial t
  :depends-on (:common-lisp)
  :components (
  (:module "closer-mop"
    (:module "pcl"
      (:file "closer-mop.lisp")))))

(defsystem :defsystem-compatibility
  :serial t
  :depends-on (:common-lisp)
  :components ())

(defsystem :metabang-dynamic-classes
  :serial t
  :depends-on (:common-lisp)
  :components ())

(defsystem :fset
  :serial t
  :depends-on (:common-lisp :gmap :new-let :lexical-contexts)
  :components (
  (:module "fset"
    (:module "Code"
      (:file "bounded-sets.lisp")
      (:file "complement-sets.lisp")
      (:file "relations.lisp")
      (:file "interval.lisp")
      (:file "testing.lisp")
      (:file "reader.lisp")
      (:file "tuples.lisp")
      (:file "fset.lisp")
      (:file "wb-trees.lisp")
      (:file "order.lisp")
      (:file "port.lisp")))))

(defsystem :html-template
  :serial t
  :depends-on (:common-lisp)
  :components ())

(defsystem :imago
  :serial t
  :depends-on (:common-lisp)
  :components (
  (:module "imago"
    (:file "utilities.lisp")
    (:file "color.lisp")
    (:file "image.lisp")
    (:file "image-utilities.lisp")
    (:file "crc32.lisp")
    (:file "drawing.lisp")
    (:file "convert.lisp")
    (:file "convolve.lisp")
    (:file "compose.lisp")
    (:file "operations.lisp")
    (:file "file.lisp")
    (:file "file-png.lisp")
    (:file "file-pnm.lisp")
    (:file "file-tga.lisp")
    (:file "file-pcx.lisp"))))

(defsystem :split-sequence
  :serial t
  :depends-on (:common-lisp :cl-utilities)
  :components ())

(defsystem :lisp-markup-language-2
  :serial t
  :depends-on (:common-lisp :kmrcl)
  :components ())

(defsystem :metabang.moptilities
  :description "Moptilities builds on the Lisp Meta-Object Protocol (**MOP**)."
  :serial t
  :depends-on (:closer-common-lisp)
  :components ())

(defsystem :png-read
  :serial t
  :depends-on (:common-lisp :iterate :chipz :babel)
  :components (
  (:module "png-read"
    (:file "png-state.lisp")
    (:file "deinterlace.lisp")
    (:file "decode.lisp")
    (:file "crc.lisp")
    (:file "critical-chunks.lisp")
    (:file "ancillary-chunks.lisp")
    (:file "basic-chunks.lisp"))))

(defsystem :salza2
  :serial t
  :depends-on (:common-lisp)
  :components ())

(defsystem :trivial-gray-streams
  :serial t
  :depends-on (:common-lisp)
  :components (
  (:module "trivial-gray-streams"
    (:file "mixin.lisp"))))

(defsystem :vecto
  :serial t
  :depends-on (:common-lisp)
  :components ())

(defsystem :zlib
  :serial t
  :depends-on (:common-lisp)
  :components ())

(defsystem :zpng
  :serial t
  :depends-on (:common-lisp :salza2)
  :components ())

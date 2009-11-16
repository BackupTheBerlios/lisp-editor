
(require :iterate)

(load "../lisp-umac/generic.lisp")
(load "tools/plist-slot.lisp")

(load "tools/package-stuff.lisp")

(load "../lisp-umac/denest.lisp")

(load "tools/expression-hook.lisp")
(load "tools/expression-scan.lisp")

(load "tools/example.lisp")

(require :lml2)

(load "tools/mk-website.lisp")
(load "tools/autodoc.lisp")

(asdf:oos 'asdf:load-op :cl-gtk2-gtk)

(load "gtk/gtk-stuff.lisp")
(load "gtk/access.lisp")

(load "gtk/exemplary.lisp")

(defun load-example (name)
  (case name
    (:expr-scan (load "exemplars/expr-scan.lisp"))))

(require :cl-fad)
(load "gtk/tiler.lisp")

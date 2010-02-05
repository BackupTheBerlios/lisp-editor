
(require :gil)
(require :expression-scan)
(load "tools/autodoc-gil.lisp")

(let*((gil:*lang* :html) ;NOTE Figure out the next one if it doesn't exist..
      (*default-pathname-defaults* #p"/home/jasper/proj/lisp-editor/doc/autodoc/")
      (by-names '(defvar defun defmacro)))
  (with-open-file (*standard-output* "doc.html" :direction :output
		   :if-exists :supersede :if-does-not-exist :create)
    (flet ((doc (what &optional (tp :sys))
	     (funcall (autodoc-gil:document what tp
			 :level 3))))
      (doc :generic) ;NOTE 
      (doc :expression-scan)
      (expr-scan::scan-file "tools/autodoc-gil.lisp")
      (doc :autodoc-gil :pkg))))


;;Old
;Make sure autodoc is available.
;
;(asdf:operate 'asdf:load-op :autodoc)
;
;Document.
;(autodoc:document-system :autodoc :directory "doc/autodoc/"
; :scan-recurse-cnt 10 :write-recurse-cnt 10)


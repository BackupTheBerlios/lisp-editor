
;Make sure autodoc is available.

(asdf:operate 'asdf:load-op :autodoc)

;Document.
(autodoc:document-system :autodoc :directory "doc/autodoc/"
 :scan-recurse-cnt 10 :write-recurse-cnt 10)



(pushnew "/home/jasper/proj/lisp-umac/" 
	 asdf:*central-registry* :test #'equal)

(pushnew "/home/jasper/proj/lisp-ed/tools" 
	 asdf:*central-registry* :test #'equal)
(pushnew "/home/jasper/proj/lisp-ed/exemplars"
	 asdf:*central-registry* :test #'equal)
(pushnew "/home/jasper/proj/lisp-ed/gtk"
	 asdf:*central-registry* :test #'equal)
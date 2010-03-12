
;;Assumes current directory is at base of project.

(pushnew "tools/" asdf:*central-registry* :test #'equal)
(pushnew "gil/" asdf:*central-registry* :test #'equal)
(pushnew "libs/" asdf:*central-registry* :test #'equal)

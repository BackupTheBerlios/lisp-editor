;;
;;  Copyright (C) 05-03-2010 Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;

(cl:in-package :cl-user)

(in-package :gil-store-log)

;;'playtesting' gil-store-log.

;;;TODO seems to ignnore filename..
(with-log (log (make-instance 'gil-log :file "/home/jasper/proj/lisp-editor/tools/test/log/logfile"
		  :root "/home/jasper/proj/lisp-editor/tools/test/log/"))
  (add-entry log :new-files))

(with-log (log "tools/test/log/logfile")
  (map-entries log (entry file)
    (declare (ignore entry))
    (print file)))

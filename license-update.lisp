
;;Little file that updates the licenses.

(require :cl-fad)

(defun update-license (file)
  "Updates the license dates.
 WARNING JUST OVERWRITES THE START!! MAKE SURE INITIAL ALREADY THERE"
  (multiple-value-bind (second minute hour date month year)
      (get-decoded-time)
    (declare (ignore second minute hour))
    (with-open-file (stream file :direction :output
		     :if-does-not-exist :error :if-exists :overwrite)
      (format stream
";;
;;  Copyright (C) ~a-~a-~a Jasper den Ouden.
;;
;;  This is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;"
(if (< date 10) (format nil "0~a" date) date)
(if (< month 10) (format nil "0~a" month) month)
year)))) ;Note: update before year 10000

(defun update-licenses-in (directory)
  (let ((*default-pathname-defaults*
	 (pathname
	  (format nil "~a~a" 
		  (directory-namestring *default-pathname-defaults*)
		  directory))))
    (mapcar (lambda (file &key (filename (file-namestring file))
		               (len (length filename)))
	      (when (and (> len 5)
			 (string= filename ".lisp" :start1 (- len 5)))
		(update-license filename)))
	    (cl-fad:list-directory "."))))

(update-licenses-in "gil/")
(update-licenses-in "gil/tools/")
(update-licenses-in "gil/output/")

(update-licenses-in "tools/")
(update-licenses-in "gtk/")
(update-licenses-in "log/")
(update-licenses-in "exemplars/")

(update-license "test-file")

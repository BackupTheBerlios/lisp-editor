
(in-package #:lisp-ed)

(defun file-operations (text)
  "Bindings for saving and writing texts to files.
TODO 1) no regexpr yet 2) file doesn't go to correct buffer."
  (assure-event-mode :x)
  (add-moded-event :x "<Control-Key-f>" (to-entry text :open-file))
  (add-moded-event :x "<Control-Key-s>" ;Direct saving.
    (lambda (evt)
      (declare (ignore evt))
      (with-slots (filename) text
	(if (= (length filename) 0)
	  (error "TODO message when no name for file.")
	  (save-text text filename)))))
  (with-binding text
    (bind-activate "<Control-Key-w>"))
  (add-moded-event :x "<Control-Key-w>" (to-entry text :save-file)))

;TODO bookmarks
;(defun bookmarks (text)
;  )

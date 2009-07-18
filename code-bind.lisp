
(in-package :lisp-ed)

(defun package-at-line (text-str line)
  "TODO Determines in which package is at the given line."
  "CL-USER")

(defun evaluate-binding (text)
  "Evaluates, while scanning the code under the text.
TODO get package right"
  (with-binding text
    (bind-obj "<Alt-Key-x>"
     (lambda (text) ;Set macroexpand hook, current line up.
       ;;TODO go up to 'orignal' line.
       (let ((code-scan:*cur-line* (get-cursor-line text))
	     (*macroexpand-hook* #'code-scan:scanning-macrohook)
	     (code-scan:*cur-file* (slot-value text 'buffer-name)))
	 (setf (text *entry*) ;Just eval, and let scanning do it's job.
	       (format nil "~D"
		       (eval (read-from-string
			      (get-current-expression text))))))))))

(defun show-fun (symbol &key (canvas (make-instance 'canvas)))
  "Shows information on the function/macro refered to by symbol.
Currently, the documentation string, if any, and the graph of relations\
 with other functions. ;TODO how about also definition?
TODO allow key bindings to this window, and make some."
  (with-ltk ()
    (with-slots (code-scan:doc code-scan:args)
	(gethash symbol code-scan:*functions*)
    ;Display name, arguments and documentation.
      (pack (make-instance 'label
	      :name (format nil "~D ~D~%~D"
			    symbol code-scan:args code-scan:doc))
	    :side :top)
    ;And display visualization of relations.
      (pack canvas :side :bottom)
      (let*((by-dist (down-graph:list-by-dist from graph depth))
	    (rect-by-dist
	     (down-graph-draw:get-rect-positions by-dist
	       :predicate (lambda(el) ;TODO improve.
			    (let ((name (package-name (symbol-package el))))
			      (not (string= name "COMMON-LISP"))))))
	    (graph (car code-scan:*function-links*)))
       ;Optimize it for the graphical representation somewhat.
	(down-graph-draw:optimize-rect-by-dist rect-by-dist graph)
	(down-graph-draw:ltk-draw-from rect-by-dist graph
				       :canvas canvas)))))

(defun go-to-fun (symbol)
  "Go to the definition of the function in the file.(As close as the\
 scanner finds, though."
  (with-slots (code-scan:from-line code-scan:from-file)
      (gethash symbol code-scan:*functions*)
    (go-to-file code-scan:from-file)
    (go-to-line code-scan:from-line)))

(defun show-fun-binding (text &key (m-q "<Alt-Key-q>") (m-g "<Alt-Key-g>"))
  "Querries information on the current function"
  (with-binding text
    (bind-obj m-q ;Querring.
      (lambda (text)
	(show-fun
	 (intern (get-current-word text)
		 (package-at (text text) (get-cursor-line text))))))
    (bind-obj m-g ;Going to the file.
      (lambda (text)
	(go-to-fun
	 (intern (get-current-word text)
		 (package-at (text text) (get-cursor-line text))))))))

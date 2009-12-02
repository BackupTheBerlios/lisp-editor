
(in-package :gil-txt) ;;TODO make test package, export symbols.

(defun test (&key (lang :txt) standard-output
	     (output (if standard-output *standard-output*
		       (make-string-output-stream))))
  (let ((*lang* lang) (*standard-output* output))
    (funcall
     (p
      (header 2 "Loose header")
      (section 1 "Main" "Main"
	       "This is a text in a section. " "And some more."
	       (point-list
		"Item" "Item" "Item" 
		(alt-point-list "A" "B" "C")
"Last item
with two lines"))
      (p (b "brains") " eat them. ")
      (p (b "Eat them good")". " "Cookies too!")))
    (get-output-stream-string output)))

(test)

(test :lang :html)

(let ((*lang* :html))
  (funcall
   (p
    (header 2 "Loose header")
    (section 1 "Main" "Main"
	     "This is a text in a section. " (b "And some more.")
	     (point-list
	      "Item" "Item" "Item" 
	      (alt-point-list "A" "B" "C"))))))
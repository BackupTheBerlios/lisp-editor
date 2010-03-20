(cl:in-package :cl-user)

(defpackage :gil-test
  (:use :cl :gil :gil-share)
  (:documentation "Use-testing of gil stuff."))

;;Messy bunch

(in-package :gil-test)

(defun test (&key (lang :txt) standard-output
	     (output (if standard-output *standard-output*
		       (make-string-output-stream))))
  (let ((*lang* lang) (*standard-output* output))
    (funcall
     (series
      (header 2 "Loose header")
      "Really long texts that continues on and on and on and on on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on."
      (section 1 "Main" "Main"
	       "This is a text in a section. " "And some more."
	       (numbered-list
		"Item" "Item" "Item" 
		(alt-point-list "A" "B" "C" "monkey
lalala")
"Last item
with two lines"))
      (p (b "bold stuff") "not bold. ")
      (p (b "Bold again")". " "Mention of cookies!")
      (make-instance 'file-image :filename "http://www.w3schools.com/images/pulpit.jpg")
      (section 0 "ofile" "Section possibly in other file"
	       "Text." " More text.")))
    (when (eql (type-of output) 'SB-IMPL::STRING-OUTPUT-STREAM)
      (get-output-stream-string output))))

(test :standard-output t :lang :latex)

(with-open-file (str "first.html" :direction :output
		 :if-does-not-exist :create :if-exists :supersede)
  (test :lang :html :output str))

;TODO fix to match changes.
(with-open-file (str "first.txt" :direction :output
		 :if-does-not-exist :create :if-exists :supersede)
  (test :lang :txt :output str))

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

(let ((*lang* :html))
  (funcall
   (p
    (header 2 "Loose header")
    (section 1 "Main" "Main"
	     "This is a text in a section. " (b "And some more.")
	     (point-list
	      "Item" "Item" "Item" 
	      (alt-point-list "A" "B" "C"))))))

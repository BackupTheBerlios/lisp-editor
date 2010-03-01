
(cl:in-package :gil-read)

(dotimes (k 1000)
  (denest
   (let ((n (random 10000000000))))
   (with-stream (stream (format nil "~D" n)))
   (let ((m (read-number stream))))
   (unless (= n m)
     (error "Incorrect read: ~D ~D" n m))))

(dotimes (k 1000)
  (denest
   (let ((n (round (random 1000.0) 0.00001)))) ;Rounded.
   (with-stream (stream (format nil "~D" n)))
   (let ((m (read-number stream))))
   (unless (= n m)
     (error "Incorrect read: ~D ~D" n m))))


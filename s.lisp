(require :ltk)

(in-package #:ltk)

(defun 

(defun hello-1()
  (with-ltk ()
   (let ((b (make-instance 'button
                           :master nil
                           :text "Press Me"
                           :command (lambda ()
                                      (format t "Hello World!~&")))))
     (pack b)
     (pack (make-instance 'text) :side :bottom)
     (pack (make-instance 'text) :side :left))))


(hello-1)


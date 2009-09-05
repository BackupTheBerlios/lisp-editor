(in-package :mk-website)


;;NOTE old stuff, doesn't work.

(html-file-page ("doc/miauw")
  (with-website-bars "website"))

(html-file-page ("doc/miauw")
  (with-bars (()
	      :top ((:height "30%") (:td "banner")) :left (() "menu")
	      :bottom (() (:td "stuff")))
    "miauw"))

(with-bars (:top (() ("banner")) :left (() ("miaw"))) "lala"))

(html-file-page ("doc/main")
  (html
   (:head (:title "Lang main page")
    ((:meta :http-equiv "Content-Type" :content "text/html; charset=iso-8859-1"))
    ((:meta :name "description" :content "Lang web/documentation pages."))
    ((:meta :name "author" :content "Jasper den Ouden"))
    ((:meta :name "keywords" :content "Lisp, Computer Language")))
   (:body
 
(html-file-page ("doc/miauw")   
  (html((:table :cellpadding 3)
     (:tbody
      (:tr
       ((:td :colspan 0)
	"Iteration")
       ((:td "stuff")))
      (:td 
       "a") (:td "b")))))



(cl:in-package :lisp-editor-autodoc)

(document-log-data :gil-log
  :rss-file "The rss file to write to."
  :log-title "Title of log, also of RSS."
  :log-link "Link of RSS."
  :log-description "Description of RSS feed."
  :categories "Categories of RSS feed.(Not automatically taken from files,\
 atm, perhaps it should be able of doing it.)"
  :last-rss-change
  "Last change to RSS, one that warrants changing pubdate."
  :last-rss-write "Last time written too RSS."
  :last-update-time "Last time anything was updated.")

(document-log-data :gil-log
  :title "Title of news item."
  :link "Link as url." :link-name "Link as gil name."
  :description "Description of item."
  :notable "Notable words, used for categories."
  :author "Author"
  :comments-thread "Comments url"
  :comments-thread-name "Comments gil name."

  :first-update-time "First time updated"
  :last-update-time "Last time updated."
  :last-execute-time "Last time executed.")

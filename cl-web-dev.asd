;;;; cl-web-dev.asd

(asdf:defsystem #:cl-web-dev
  :serial t
  :description "Describe cl-web-dev here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot
               #:parenscript
               #:cl-who)
  :components ((:file "package")
               (:file "cl-web-dev")))


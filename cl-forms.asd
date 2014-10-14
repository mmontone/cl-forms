;;;; cl-forms.asd

(asdf:defsystem #:cl-forms
  :serial t
  :description "Describe cl-forms here"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-ppcre
               #:hunchentoot)
  :components ((:file "package")
               (:file "cl-forms")))


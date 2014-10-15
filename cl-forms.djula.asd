(asdf:defsystem #:cl-forms.djula
  :serial t
  :description "Describe cl-forms here"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cl-forms
	       #:djula)
  :components ((:file "djula")))

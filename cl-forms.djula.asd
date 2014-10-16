(asdf:defsystem #:cl-forms.djula
  :serial t
  :description "Describe cl-forms here"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cl-forms
	       #:cl-forms.who
	       #:djula)
  :components ((:file "djula")))

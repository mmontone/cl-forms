(asdf:defsystem #:cl-forms.who
  :serial t
  :description "Describe cl-forms here"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cl-forms
	       #:cl-who)
  :components ((:file "who")))

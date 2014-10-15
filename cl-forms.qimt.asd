(asdf:defsystem #:cl-forms.qimt
  :serial t
  :description "Describe cl-forms here"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cl-forms
	       #:qimt)
  :components ((:file "qimt")))

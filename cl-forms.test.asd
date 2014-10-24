(asdf:defsystem #:cl-forms.test
  :serial t
  :description "Describe cl-forms here"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cl-forms
	       #:fiveam)
  :components ((:module :test
			:components
			((:file "package")
			 (:file "test")))))

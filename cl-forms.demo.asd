(asdf:defsystem #:cl-forms.demo
  :serial t
  :description "Describe cl-forms here"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cl-forms 
	       #:cl-forms.who 
	       #:cl-forms.test 
	       #:hunchentoot
	       #:cl-who)
  :components ((:module :test
                        :components
                        ((:file "package")
                         (:file "demo")
			 (:module :demo-tests
				  :pathname "demo"
				  :components
				  ((:file "fields")
				   (:file "models")
				   (:file "tests"))
				  :serial t))
			:serial t)))

(asdf:defsystem #:cl-forms.demo
  :serial t
  :description "Describe cl-forms here"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cl-forms 
	       #:cl-forms.who 
	       #:fiveam 
	       #:hunchentoot
	       #:cl-who)
  :components ((:module :test
                        :components
                        ((:file "package")
                         (:file "demo")))))

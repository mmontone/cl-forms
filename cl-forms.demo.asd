(asdf:defsystem #:cl-forms.demo
  :serial t
  :description "CL-FORM demo application"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cl-forms
               #:cl-forms.who
               #:cl-forms.who.bootstrap
	       #:djula
	       #:cl-forms.djula
               #:cl-forms.test
               #:hunchentoot
               #:cl-who
               #:cl-css)
  :components ((:module :test
                        :components
                        ((:file "package")
                         (:file "demo")
                         (:module :demo-tests
                                  :pathname "demo"
                                  :components
                                  ((:file "fields")
                                   (:file "models")
                                   (:file "validation")
                                   (:file "client-validation")
                                   (:file "layout")
                                   (:file "form-templates")
                                   (:file "themes")
                                   (:file "renderers")
                                   (:file "composition")
                                   (:file "tests"))
                                  :serial t))
                        :serial t)))

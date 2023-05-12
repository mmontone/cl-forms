(asdf:defsystem #:cl-forms.test
  :serial t
  :description "CL-FORMS tests"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cl-forms
               #:fiveam)
  :components ((:module :test
                        :components
                        ((:file "package")
                         (:file "test"))))
  :perform (asdf:test-op (o c)
                         (uiop:symbol-call :cl-forms.test :run-tests)))

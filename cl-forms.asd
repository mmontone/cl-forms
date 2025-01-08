(asdf:defsystem #:cl-forms
  :serial t
  :description "A web forms handling library"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mmontone/cl-forms"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :depends-on (#:hunchentoot #:cl-forms.core)
  :components ((:module :src
                        :components
                        ((:file "hunchentoot"))))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-forms.test))))

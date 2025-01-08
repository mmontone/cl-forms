(asdf:defsystem #:cl-forms.core
  :serial t
  :description "A web forms handling library"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mmontone/cl-forms"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :depends-on (#:alexandria
               #:cl-ppcre
               #:ironclad
               #:uuid
               #:clavier
               #:fmt
	       #:str)
  :components ((:module :src
                        :components
                        ((:file "package")
			 (:module :themes
                                  :components
                                  ((:file "theme")
                                   (:file "default")
                                        ;(:file "bootstrap")
                                   (:file "specials"))
                                  :serial t)
                         (:file "cl-forms")
                         (:module :fields
                                  :components
                                  ((:file "string")
                                   (:file "boolean")
                                   (:file "email")
                                   (:file "password")
                                   (:file "url")
                                   (:file "integer")
                                   (:file "choice")
                                   (:file "date")
                                   (:file "datetime")
                                   (:file "file")
                                   (:file "hidden")
                                   (:file "submit")
                                   (:file "subform")
                                   (:file "list"))))
                        :serial t))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-forms.test))))

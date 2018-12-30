(asdf:defsystem #:cl-forms
  :serial t
  :description "A web forms handling library"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-ppcre
               #:hunchentoot
               #:ironclad
               #:uuid
               #:clavier
               #:fmt)
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

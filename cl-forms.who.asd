(asdf:defsystem #:cl-forms.who
  :serial t
  :description "Describe cl-forms here"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cl-forms
               #:cl-who)
  :components ((:module :renderer
                        :pathname "src/renderer/"
                        :components
                        ((:file "who")))))

(asdf:defsystem #:cl-forms.who.bootstrap
  :serial t
  :description "Describe cl-forms here"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (:cl-forms.who)
  :components ((:module :src
                        :components
                        ((:file "package")
                         (:module :themes
                                  :components
                                  ((:file "bootstrap")))))))

(asdf:defsystem #:cl-forms.who.bootstrap
  :serial t
  :description "Bootstrap theme for CL-FORMS via CL-WHO renderer"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (:cl-forms.who)
  :components ((:module :src
                        :components
                        ((:file "package")
                         (:module :themes
                                  :components
                                  ((:file "bootstrap")))))))

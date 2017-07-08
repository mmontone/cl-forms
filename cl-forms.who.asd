(asdf:defsystem #:cl-forms.who
  :serial t
  :description "CL-FORMS CL-WHO backend"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cl-forms
               #:cl-who
               #:cl-css)
  :components ((:module :renderer
                        :pathname "src/renderer/"
                        :components
                        ((:file "who")))))

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

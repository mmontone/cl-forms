(asdf:defsystem #:cl-forms.who
  :serial t
  :description "CL-FORMS CL-WHO backend"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cl-forms.core
               #:cl-who)
  :components ((:module :renderer
                        :pathname "src/renderer/"
                        :components
                        ((:file "who")))))

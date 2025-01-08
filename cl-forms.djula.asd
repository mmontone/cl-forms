(asdf:defsystem #:cl-forms.djula
  :serial t
  :description "CL-FORMS Djula backend"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cl-forms.core
               #:cl-forms.who
               #:djula)
  :components ((:module :renderer
                        :pathname "src/renderer/"
                        :components
                        ((:file "djula")))))

(asdf:defsystem #:cl-forms.peppol
  :serial t
  :description "Some extra form fields (country, currency, amount) using PEPPOL billing code lists"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:cl-forms
               #:peppol)
  :components ((:module :src
                        :components
                        ((:file "peppol")))))

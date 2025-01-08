(asdf:defsystem #:cl-forms.ningle
  :serial t
  :description "CL-FORMS ningle backend"
  :author "Neil Munro"
  :license "MIT"
  :depends-on (#:lack
               #:lack/middleware/csrf
               #:ningle
               #:cl-forms.core)
  :components ((:module :src
                        :components
                        ((:file "ningle")))))

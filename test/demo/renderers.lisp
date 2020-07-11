(in-package :forms.test)

(hunchentoot:define-easy-handler (demo-renderers :uri "/renderers") ()
  (flet ((todo ()
           (who:with-html-output (forms.who::*html*)
             (:p
              "TODO: show an example of forms rendered with different renderers here (cl-who, Djula, etc)"))))
    (render-demo-page :demo #'todo
                      :source (asdf:system-relative-pathname :cl-forms.demo
                                                             "test/demo/renderers.lisp")
                      :active-menu :renderers)))

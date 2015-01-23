(in-package :forms.test)

(hunchentoot:define-easy-handler (fields-demo-handler :uri "/fields") ()
  (render-demo-page :demo #'fields-demo
		    :source (asdf:system-relative-pathname :cl-forms.demo 
							   "test/demo/fields.lisp")
		    :active-menu :fields))

(defun fields-demo ()
  (who:with-html-output (*html*)
    (:p (who:str "Fields"))))



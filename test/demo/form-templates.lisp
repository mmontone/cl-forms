(in-package :forms.test)

(defun form-template-demo ()
  (macrolet ((row (&body body)
               `(who:htm
                 (:div :class "row"
                       (who:htm
                        ,@body))))
             (col (&body body)
               `(who:htm
                 (:div :class "col-md-2"
                       (who:htm
                        ,@body)))))
    (forms:with-form-renderer :who
      (who:with-html-output (forms.who::*html*)
        (:div :class :container
              (:div :class :row
                    (:div :class :heading
                          (:h1 (who:str "Form templates")))
                    (:p (who:str "Form templates is an alternative way of defining and rendering forms. Instead of defining a form with defform and then specifiying a template and render it, forms templates allow to do all that at the same time."))
                    (:p (who:str "Form definition is embedded in rendering spec via with-form-template. See source for details."))                
                    (:div :class :container
                          (forms:with-form-template () template-form (:action "/template-post")
                            (row
                             (:h3 (who:str "General"))
                             (col (form-field firstname :string :value ""))
                             (col (form-field lastname :string :value "")))
                            (form-field active :boolean :value t)
                            (row
                             (:h3 (who:str "Address"))
                             (form-field address :string :value ""))
                            (row
                             (:h3 (who:str "Other"))
                             (form-field choices :choice
                                         :choices (list "Foo" "Bar")
                                         :value (list "Foo")
                                         :multiple t)
                             (form-field choices2 :choice
                                         :choices (list "Foo" "Bar")
                                         :value (list "Bar")
                                         :multiple t
                                         :expanded  t))
                            (row
                             (form-field submit :submit :label "Create"))))))))))

(hunchentoot:define-easy-handler (template-demo-handler :uri "/template") ()
  (render-demo-page :demo #'form-template-demo
                    :source (asdf:system-relative-pathname :cl-forms.demo
                                                           "test/demo/form-templates.lisp")
                    :active-menu :template))

(hunchentoot:define-easy-handler (template-form-post
                                  :uri "/template-post"
                                  :default-request-type :post) ()
  (flet ((fields-post ()
           (let ((form (forms:find-form 'template-form)))
             (forms::handle-request form)
             (if (forms::validate-form form)
                 (forms::with-form-field-values (firstname lastname active address
                                                           choices choices2) form
                   (who:with-html-output (forms.who::*html*)
                     (:ul
                      (:li (who:fmt "Firstname: ~A" firstname))
                      (:li (who:fmt "Lastname: ~A" lastname))
                      (:li (who:fmt "Active: ~A" active))
                      (:li (who:fmt "Address: ~A" address))
                      (:li (who:fmt "Choices: ~A" choices))
                      (:li (who:fmt "Choices2: ~A" choices2)))))
                 "Form is not valid"))))
    (render-demo-page :demo #'fields-post
                      :source (asdf:system-relative-pathname :cl-forms.demo
                                                             "test/demo/form-templates.lisp")
                      :active-menu :template)))

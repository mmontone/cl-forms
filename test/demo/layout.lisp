(in-package :forms.test)

(forms:defform layout-form (:action "/layout-post")
  ((firstname :string :value "")
   (lastname :string :value "")
   (active :boolean :value t)
   (address :string :value "")
   (choices :choice
            :choices (list "Foo" "Bar")
            :value (list "Foo")
            :multiple t)
   (choices2 :choice
             :choices (list "Foo" "Bar")
             :value (list "Bar")
             :multiple t
             :expanded  t)
   (submit :submit :label "Create")))

(defun layout-demo ()
  (who:with-html-output (forms.who::*html*)
    (:div :class :container
          (:div :class :row
                (:div :class :heading
                      (:h1 (who:str "Custom layed-out form")))
                (:p (who:str "This is a form with a custom layout:"))
                (let ((form (forms::get-form 'layout-form)))
                  (macrolet ((f (name)
                               `(forms:render-field ',name))
                             (row (&body body)
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
                      (forms:with-form form
                        (who:with-html-output (forms.who::*html*)
                          (:div :class :container
                                (forms:render-form-start)
                                (row 
                                 (:h3 (who:str "General"))
                                 (col (f firstname)) (col (f lastname)))
                                (f active)
                                (row
                                 (:h3 (who:str "Address"))
                                 (f address))
                                (row                          
                                 (:h3 (who:str "Other"))
                                 (f choices)
                                 (f choices2))
                                (row
                                 (f submit))
                                (forms:render-form-end)))))))))))

(hunchentoot:define-easy-handler (layout-demo-handler :uri "/layout") ()
  (render-demo-page :demo #'layout-demo
                    :source (asdf:system-relative-pathname :cl-forms.demo
                                                           "test/demo/layout.lisp")
                    :active-menu :layout))

(hunchentoot:define-easy-handler (layout-form-post
                                  :uri "/layout-post"
                                  :default-request-type :post) ()
  (flet ((fields-post ()
           (let ((form (forms:get-form 'fields-form)))
             (forms::handle-request form)
             (if (forms::validate-form form)
                 (forms::with-form-field-values (firstname lastname active address
                                                 sex choices choices2) form
                   (who:with-html-output (forms.who::*html*)
                     (:ul
                      (:li (who:fmt "Firstname: ~A" firstname))
                  (:li (who:fmt "Lastname: ~A" lastname))
                  (:li (who:fmt "Active: ~A" active))
                  (:li (who:fmt "Sex: ~A" sex))
                  (:li (who:fmt "Address: ~A" address))
                  (:li (who:fmt "Choices: ~A" choices))
                  (:li (who:fmt "Choices2: ~A" choices2)))))
                 "Form is not valid"))))
    (render-demo-page :demo #'fields-post
                      :source (asdf:system-relative-pathname :cl-forms.demo
                                                             "test/demo/fields.lisp")
                      :active-menu :layout)))

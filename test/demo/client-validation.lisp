(in-package :cl-forms.test)

(forms:defform client-validated-form (:action "/client-validation/post"
                                              :client-validation t)
  ((name :string :value "" :constraints (list (clavier:is-a-string)
                                              (clavier:not-blank)
                                              (clavier:len :max 5))
         :validation-triggers '(:focusin))
   (single :boolean :value t)
   (sex :choice :choices (list "Male" "Female") :value "Male")
   (age :integer :constraints (list (clavier:is-an-integer)
                                    (clavier:greater-than -1)
                                    (clavier:less-than 200)))
   (email :email)
   (submit :submit :label "Create")))

(defun client-validation (&optional form)
  (let ((form (or form (forms::find-form 'client-validated-form))))
    (forms:with-form-renderer :who
      (who:with-html-output (forms.who::*html*)
        (:h1 (who:str "Client side validation"))
        (:p (who:str "This is an example of how client side validation works. Client side validation uses parsleyjs library for validating client side."))
        (:p (who:str "The interesting thing about the implementation is that validations are specified in the form definition, and are \"compiled\" to rules in javascript. Also, this example uses the exactly the same constraints than the server side validation demo."))      
        (forms:render-form form)))))

(hunchentoot:define-easy-handler (client-validation-handler
                                  :uri "/client-validation") ()
  (render-demo-page :demo #'client-validation
                    :source (asdf:system-relative-pathname :cl-forms.demo
                                                           "test/demo/client-validation.lisp")
                    :active-menu :client-validation))

(hunchentoot:define-easy-handler (client-validation-post :uri "/client-validation/post" :default-request-type :post) ()
  (flet ((client-validation-post ()
           (let ((form (forms:find-form 'client-validated-form)))
             (forms::handle-request form)
             (if (forms::validate-form form)
                 ;; The form is valid
                 (forms::with-form-field-values (name single sex age email) form
                   (who:with-html-output (forms.who::*html*)
                     (:ul
                      (:li (who:fmt "Name: ~A" name))
                      (:li (who:fmt "Single: ~A" single))
                      (:li (who:fmt "Sex: ~A" sex))
                      (:li (who:fmt "Age: ~A" age))
                      (:li (who:fmt "Email: ~A" email)))))
                 ;; The form is not valid
                 (client-validation form)))))
    (render-demo-page :demo #'client-validation-post
                      :source (asdf:system-relative-pathname :cl-forms.demo
                                                             "test/demo/client-validation.lisp")
                      :active-menu :client-validation)))

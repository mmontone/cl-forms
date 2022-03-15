(in-package :forms.test)

(forms:defform bs-fields-form (:action "/bs-fields-post" :csrf-protection nil)
  ((name :string :value "")
   (ready :boolean :value t)
   (sex :choice :choices (list "Male" "Female") :value "Male")
   (submit :submit :label "Create")))

(defun bootstrap-form-demo ()
  (let ((form (forms::find-form 'bs-fields-form)))
    (forms:with-form-theme 'forms.who::bootstrap-form-theme
      (forms:with-form-renderer :who
        (who:with-html-output (forms.who::*html*)
          (:h1 (who:str "Bootstrap theme"))
          (:h2 (who:str "Vertical (default)"))
          (forms:render-form form)
          (:h2 (who:str "Inline"))
          (forms:render-form form :inline t)
          (:h2 (who:str "Horizontal"))
          (forms:render-form form :horizontal t))))))

(hunchentoot:define-easy-handler (bootstrap-form :uri "/themes") ()
  (render-demo-page :demo #'bootstrap-form-demo
                    :source (asdf:system-relative-pathname :cl-forms.demo 
                                                           "test/demo/themes.lisp")
                    :active-menu :themes))

(hunchentoot:define-easy-handler (bs-fields-form-post 
                                  :uri "/bs-fields-post" 
                                  :default-request-type :post) ()
  (flet ((fields-post ()
           (let ((form (forms:find-form 'bs-fields-form)))
             (forms::handle-request form)
             (forms::with-form-fields (name ready sex) form
               (who:with-html-output (forms.who::*html*)
                 (:ul 
                  (:li (who:fmt "Name: ~A" (forms::field-value name)))
                  (:li (who:fmt "Ready: ~A" (forms::field-value ready)))
                  (:li (who:fmt "Sex: ~A" (forms::field-value sex)))))))))
    (render-demo-page :demo #'fields-post
                      :source (asdf:system-relative-pathname :cl-forms.demo 
                                                           "test/demo/themes.lisp")
                      :active-menu :themes)))

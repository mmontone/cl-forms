(defpackage :forms.test.who
  (:use :cl :forms))

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 2021))

(forms:defform simple-form (:action "/post")
  ((name :string :value "")
   (ready :boolean :value t)
   (submit :submit :label "Create")))

(hunchentoot:define-easy-handler (simple-form :uri "/") ()
  (let ((form (forms::get-form 'simple-form)))
    (forms:with-form-renderer :who
      (forms:render-form form))))

(hunchentoot:define-easy-handler (post-form :uri "/post" :default-request-type :post) ()
  (let ((form (forms:get-form 'simple-form)))
    (forms::handle-request form)
    (forms::validate-form form)
    (forms::with-form-fields (name ready) form
      (who:with-html-output-to-string (html)
	(:ul 
	 (:li (who:fmt "Name: ~A" (forms::field-value name)))
	 (:li (who:fmt "Ready: ~A" (forms::field-value ready))))))))

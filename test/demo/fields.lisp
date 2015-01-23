(in-package :forms.test)

(hunchentoot:define-easy-handler (fields-demo-handler :uri "/fields") ()
  (render-demo-page :demo #'fields-demo
		    :source (asdf:system-relative-pathname :cl-forms.demo 
							   "test/demo/fields.lisp")
		    :active-menu :fields))

(forms:defform fields-form (:action "/fields-post")
  ((name :string :value "")
   (ready :boolean :value t)
   (sex :choice :choices (list "Male" "Female") :value "Male")
   (submit :submit :label "Create")))

(hunchentoot:define-easy-handler (fields-form-post 
				  :uri "/fields-post" 
				  :default-request-type :post) ()
  (flet ((fields-post ()
	   (let ((form (forms:get-form 'fields-form)))
	     (forms::handle-request form)
	     (forms::with-form-fields (name ready sex) form
	       (who:with-html-output (forms.who::*html*)
		 (:ul 
		   (:li (who:fmt "Name: ~A" (forms::field-value name)))
		   (:li (who:fmt "Ready: ~A" (forms::field-value ready)))
		   (:li (who:fmt "Sex: ~A" (forms::field-value sex)))))))))
    (render-demo-page :demo #'fields-post
		      :source (asdf:system-relative-pathname :cl-forms.demo 
							     "test/demo/fields.lisp")
		      :active-menu :fields)))

(defun fields-demo ()
  (let ((form (forms::get-form 'fields-form)))
    (forms:with-form-renderer :who
      (forms:render-form form))))



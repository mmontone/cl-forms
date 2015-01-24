(in-package :forms.test)

(defclass person ()
  ((name :initarg :name
	 :accessor person-name
	 :initform nil)
   (single :initarg :single
	   :accessor person-single
	   :initform t)
   (sex :initarg :sex
	:accessor person-sex
	:initform :male)))

(forms:defform-builder model-form (person)
  (make-instance 'forms::form
		 :name 'model-form
		 :model person
		 :action "/models-post"
		 :fields (forms::make-form-fields
			  `((name :string :label "Name"
				  :accessor person-name)
			    (single :boolean :label "Single"
				    :accessor person-single)
			    (sex :choice :label "Sex"
				 :choices (:male :female)
				 :accessor person-sex
				 :formatter format-sex)
			    (submit :submit :label "Update")))))

(defun format-sex (sex)
  (if (equalp sex :male) "Male" "Female"))

(defun models-demo ()
  (let ((person (make-instance 'person
			       :name "Foo"
			       :single t
			       :sex :male)))
    (let ((form (forms::get-form 'model-form person)))
      (forms:with-form-renderer :who
	(forms:render-form form)))))

(hunchentoot:define-easy-handler (model-form :uri "/models") ()
  (render-demo-page :demo #'models-demo
		    :source (asdf:system-relative-pathname :cl-forms.demo 
							   "test/demo/models.lisp")
		    :active-menu :models))

(hunchentoot:define-easy-handler (model-form-post :uri "/models-post"
						  :default-request-type :post) ()
  (flet ((model-post ()
	   (let ((person (make-instance 'person)))
	     (let ((form (forms:get-form 'model-form person)))
	       (forms::handle-request form)
	       (forms::validate-form form)
	       (who:with-html-output (forms.who::*html*)
		 (:ul 
		   (:li (who:fmt "Name: ~A" (person-name person)))
		   (:li (who:fmt "Single: ~A" (person-single person)))
		   (:li (who:fmt "Sex: ~A" (person-sex person)))))))))
    (render-demo-page :demo #'model-post
		      :source (asdf:system-relative-pathname :cl-forms.demo
							     "test/demo/models.lisp")
		      :active-menu :models)))

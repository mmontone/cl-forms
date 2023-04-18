(defpackage :cl-forms.test.djula
  (:use :cl :forms))

(in-package :cl-forms.test.djula)

(defparameter +simple-form+
  (djula:compile-template*
   (asdf:system-relative-pathname :cl-forms
				  "test/djula.html")))

(forms:defform djula-simple-form (:action "/djula/simple-form/post")
  ((name :string :value "")
   (ready :boolean :value t)
   (sex :choice :choices (list "Male" "Female") :value "Male")
   (submit :submit :label "Create")))

(hunchentoot:define-easy-handler (djula-simple-form :uri "/djula/simple-form") ()
  (let ((form (forms::find-form 'djula-simple-form)))
    (djula:render-template* +simple-form+ nil :form form)))

(hunchentoot:define-easy-handler (djula-simple-form-post
				  :uri "/djula/simple-form/post"
				  :default-request-type :post) ()
  (let ((form (forms:find-form 'djula-simple-form)))
    (forms::handle-request form)
    (forms::validate-form form)
    (forms::with-form-fields (name ready sex) form
      (who:with-html-output-to-string (html)
	(:ul 
	 (:li (who:fmt "Name: ~A" (forms::field-value name)))
	 (:li (who:fmt "Ready: ~A" (forms::field-value ready)))
	 (:li (who:fmt "Sex: ~A" (forms::field-value sex))))))))

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
		 :action "/model-form/post"
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

(hunchentoot:define-easy-handler (model-form :uri "/model-form") ()
  (let ((person (make-instance 'person
			       :name "Foo"
			       :single t
			       :sex :male)))
    (let ((form (forms::find-form 'model-form person)))
      (forms:with-form-renderer :who
	(forms:render-form form)))))

(hunchentoot:define-easy-handler (model-form-post :uri "/model-form/post"
						  :default-request-type :post) ()
  (let ((person (make-instance 'person)))
    (let ((form (forms:find-form 'model-form person)))
      (forms::handle-request form)
      (forms::validate-form form)
      (who:with-html-output-to-string (html)
	(:ul 
	 (:li (who:fmt "Name: ~A" (person-name person)))
	 (:li (who:fmt "Single: ~A" (person-single person)))
	 (:li (who:fmt "Sex: ~A" (person-sex person))))))))

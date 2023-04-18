(defpackage :cl-forms.test.qimt
  (:use :cl :forms :qimt :html))

(in-package :cl-forms.test.qimt)

(defmacro with-html (&body body)
  (alexandria:with-unique-names (html)
    `(with-output-to-string (,html)
       (qimt:serialize-html 
	(qimt:document
	  ,@body)
	,html))))

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 2021))

(forms:defform simple-form (:action "/qimt/simple-form/post")
  ((name :string :value "")
   (ready :boolean :value t)
   (sex :choice :choices (list "Male" "Female") :value "Male")
   (submit :submit :label "Create")))

(hunchentoot:define-easy-handler (simple-form :uri "/qimt/simple-form") ()
  (let ((form (forms::find-form 'simple-form)))
    (with-html
      (forms:with-form-renderer :qimt
	(forms:render-form form)))))

(hunchentoot:define-easy-handler (simple-form-post :uri "/qimt/simple-form/post" :default-request-type :post) ()
  (let ((form (forms:find-form 'simple-form)))
    (forms::handle-request form)
    (forms::validate-form form)
    (forms::with-form-fields (name ready sex) form
      (with-html
      	(<ul 
	 (<li (fxd "Name: ~A" (forms::field-value name)))
	 (<li (fxd "Ready: ~A" (forms::field-value ready)))
	 (<li (fxd "Sex: ~A" (forms::field-value sex))))))))

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

(hunchentoot:define-easy-handler (model-form :uri "/qimt/model-form") ()
  (let ((person (make-instance 'person
			       :name "Foo"
			       :single t
			       :sex :male)))
    (let ((form (forms::find-form 'model-form person)))
      (with-html
	(forms:with-form-renderer :qimt
	  (forms:render-form form))))))

(hunchentoot:define-easy-handler (model-form-post :uri "/model-form/post"
						  :default-request-type :post) ()
  (let ((person (make-instance 'person)))
    (let ((form (forms:find-form 'model-form person)))
      (forms::handle-request form)
      (forms::validate-form form)
      (with-html
	(<ul
	 (<li (fxd "Name: ~A" (person-name person)))
	 (<li (fxd "Single: ~A" (person-single person)))
	 (<li (fxd "Sex: ~A" (person-sex person))))))))

;; Choices widget test

(forms:defform choices-form (:action "/qimt/choices-form/post")
  ((sex :choice
	:choices (list "Male" "Female")
	:value "Male")
   (sex2 :choice
	 :choices (list "Male" "Female")
	 :value "Female"
	 :expanded t)
   (choices :choice
	    :choices (list "Foo" "Bar")
	    :value (list "Foo")
	    :multiple t)
   (choices2 :choice
	     :choices (list "Foo" "Bar")
	     :value (list "Bar")
	     :multiple t
	     :expanded  t)
   (submit :submit :label "Ok")))

(hunchentoot:define-easy-handler (choices-form :uri "/qimt/choices-form") ()
  (let ((form (forms::find-form 'choices-form)))
    (with-html
      (forms:with-form-renderer :qimt
	(forms:render-form form)))))

(hunchentoot:define-easy-handler (choices-form-post :uri "/qimt/choices-form/post" :default-request-type :post) ()
  (let ((form (forms:find-form 'choices-form)))
    (forms::handle-request form)
    (forms::validate-form form)
    (forms::with-form-field-values (sex sex2 choices choices2) form
      (with-html
	(<ul 
	 (<li (fxd "Sex: ~A" sex))
	 (<li (fxd "Sex2: ~A" sex2))
	 (<li (fxd "Choices: ~A" choices))
	 (<li (fxd "Choices2: ~A" choices2)))))))

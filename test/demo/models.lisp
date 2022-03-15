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

(defun format-sex (sex stream)
  (write-string
   (if (equalp sex :male) "Male" "Female")
   stream))

(defun models-demo ()
  (who:with-html-output (forms.who::*html*)
    (:h1 (who:str "Form models"))
    (:p "Forms can be attached to model objects. Model objects are CLOS instances from where form values are read and written to.")
    (:p "To work with models, forms are defined via defform-builder instead of defform. A form-builder is a function that takes the model objects and attaches it to the form. The form needs to define the accessors to access the model for each form field.")
    (:p "This is an example of a form attached to a person object. Please have a look at the source code to see how it is done.")
    (render-model-form)))

(defun render-model-form (&optional form)
  (let ((form (or form
                  (let ((person (make-instance 'person
                                               :name "Foo"
                                               :single t
                                               :sex :male)))
                    (forms::find-form 'model-form person)))))
    (forms:with-form-renderer :who
      (forms:render-form form))))

(hunchentoot:define-easy-handler (model-form :uri "/models") ()
  (render-demo-page :demo #'models-demo
                    :source (asdf:system-relative-pathname :cl-forms.demo
                                                           "test/demo/models.lisp")
                    :active-menu :models))

(hunchentoot:define-easy-handler (model-form-post :uri "/models-post"
                                                  :default-request-type :post) ()
  (flet ((model-post ()
           (let ((person (make-instance 'person)))
             (let ((form (forms:find-form 'model-form person)))
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

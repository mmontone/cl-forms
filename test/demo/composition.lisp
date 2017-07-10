(in-package :forms.test)

(forms:defform member-form ()
  ((name :string :value "" :required-p nil)
   (ready :boolean :value t :required-p nil)
   (sex :choice :choices (list "Male" "Female") :value "Male")))

(forms:defform composition-form (:action "/composition-post")
  (
   ;; Subforms
   (main-member :subform :subform 'member-form)
   (secondary-member :subform :subform 'member-form)
      ;; Simple list
   (todo :list :type '(:string :required-p nil)
         :empty-item-predicate (lambda (field)
                                 (let ((val (forms:field-value field)))
                                   (or (null val)
                                       (string= val "")))))
  ;; Subform list
   (members :list :type '(:subform :subform member-form)
            :empty-item-predicate (lambda (field)
                                    (let* ((subform (forms:field-value field))
                                           (val (forms:get-field-value subform 'name)))
                                      (or (null val)
                                          (string= val "")))))
   (save :submit :label "Save")))

(defun form-composition-demo (&optional form)
  (let ((form (or form (get-form 'composition-form))))
    (forms:with-form-renderer :who
      (who:with-html-output (forms.who::*html*)
        (:h1 (who:str "Forms composition"))
        (forms::render-form-start form)
        (:h2 (who:str "Subforms"))
        (forms::render-field 'main-member form)
        (forms::render-field 'secondary-member form)
        (forms::render-field 'save form)
        (:h2 (who:str "List field"))
        (forms::render-field 'todo form)
        (forms::render-field 'save form)
        (:h2 (who:str "List of subforms"))
        (forms::render-field 'members form)
        (forms::render-field 'save form)
        (forms::render-form-end form)))))

(hunchentoot:define-easy-handler (composition-demo :uri "/composition") ()
  (render-demo-page :demo #'form-composition-demo
                    :source (asdf:system-relative-pathname :cl-forms.demo 
                                                           "test/demo/composition.lisp")
                    :active-menu :composition))

(hunchentoot:define-easy-handler (composition-demo-post :uri "/composition-post") ()
  (let ((form (forms:get-form 'composition-form)))
    (forms:handle-request form)
    (render-demo-page :demo (lambda ()
                              (form-composition-demo form))
                      :source (asdf:system-relative-pathname :cl-forms.demo 
                                                             "test/demo/composition.lisp")
                      :active-menu :composition)))

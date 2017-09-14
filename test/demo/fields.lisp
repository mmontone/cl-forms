(in-package :forms.test)

(forms:defform fields-form (:action "/fields-post"
                                    :enctype "multipart/form-data")
  ((name :string :value "")
   (ready :boolean :value t)
   (sex :choice :choices (list "Male" "Female") :value "Male")
   (avatar :file :upload-handler 'handle-file-upload)
   (disabled :string :disabled-p t :required-p nil)
   (readonly :string :read-only-p t :required-p nil)
   (readonly-checkbox :boolean :read-only-p t :required-p nil)
   (disabled-checkbox :boolean :disabled-p t :required-p nil)
   (submit :submit :label "Create")))

(defun fields-demo ()
  (who:with-html-output (forms.who::*html*)
    (:h1 (who:str "Fields example"))
    (:div :class :container
          (:div :class :row
                (:div :class :heading
                      (:h3 (who:str "Simple form")))
                (let ((form (forms::get-form 'fields-form)))
                  (forms:with-form-renderer :who
                    (forms:render-form form))))
          (:div :class :row
                (:div :class :heading
                      (:h3 (who:str "Choices")))
                (let ((form (forms::get-form 'choices-form)))
                  (forms:with-form-renderer :who
                    (forms:render-form form)))))))

(hunchentoot:define-easy-handler (fields-demo-handler :uri "/fields") ()
  (render-demo-page :demo #'fields-demo
                    :source (asdf:system-relative-pathname :cl-forms.demo
                                                           "test/demo/fields.lisp")
                    :active-menu :fields))

(hunchentoot:define-easy-handler (fields-form-post
                                  :uri "/fields-post"
                                  :default-request-type :post) ()
  (flet ((fields-post ()
           (let ((form (forms:get-form 'fields-form)))
             (forms::handle-request form)
             (forms::with-form-fields (name ready sex avatar) form
               (who:with-html-output (forms.who::*html*)
                 (:ul
                  (:li (who:fmt "Name: ~A" (forms::field-value name)))
                  (:li (who:fmt "Ready: ~A" (forms::field-value ready)))
                  (:li (who:fmt "Sex: ~A" (forms::field-value sex)))
                  (:li (who:fmt "Avatar: ~A" (forms::file-name avatar))
                       (when (forms::file-name avatar)
                         (who:htm
                          (:img :width 200 :height 200
                                :src (format nil "/files?f=~A" (forms::file-name avatar))))))))))))
    (render-demo-page :demo #'fields-post
                      :source (asdf:system-relative-pathname :cl-forms.demo
                                                             "test/demo/fields.lisp")
                      :active-menu :fields)))

;; Choices widget test

(forms:defform choices-form (:action "/choices-post")
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

(hunchentoot:define-easy-handler (choices-form-post :uri "/choices-post"
                                                    :default-request-type :post) ()
  (flet ((choices-post ()
           (let ((form (forms:get-form 'choices-form)))
             (forms::handle-request form)
             (forms::validate-form form)
             (forms::with-form-field-values (sex sex2 choices choices2) form
               (who:with-html-output (forms.who::*html*)
                 (:ul
                  (:li (who:fmt "Sex: ~A" sex))
                  (:li (who:fmt "Sex2: ~A" sex2))
                  (:li (who:fmt "Choices: ~A" choices))
                  (:li (who:fmt "Choices2: ~A" choices2))))))))
    (render-demo-page :demo #'choices-post
                      :source (asdf:system-relative-pathname :cl-forms.demo
                                                             "test/demo/fields.lisp")
                      :active-menu :fields)))

;; File handling

(defvar *files* nil)
(defvar *files-path* (pathname "/tmp/cl-forms/"))

(defun handle-file-upload (file-field)
  ;; Store the file
  (let ((new-path (merge-pathnames 
                       (forms::file-name file-field)
                       *files-path*)))
    (rename-file (forms::file-path file-field)
                 (ensure-directories-exist new-path))
    ;; Save for handler
    (push (cons (forms::file-name file-field)
                (list new-path (forms::file-content-type file-field)))
          *files*)))

(defun handle-uploaded-file ()
  (let ((finfo (cdr (assoc (hunchentoot:parameter "f") *files* :test #'equalp))))
    (hunchentoot:handle-static-file (first finfo) (second finfo))))

(push 
 (hunchentoot:create-prefix-dispatcher "/files" 'handle-uploaded-file)
 hunchentoot:*dispatch-table*)

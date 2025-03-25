(defpackage :cl-forms.who
  (:nicknames :forms.who)
  (:use :cl :forms :who)
  (:export :*html*))

(in-package :forms.who)

(defvar *html* nil
  "The stream where the form is rendered to, when using the who renderer.
Make sure to bind this before rendering your form.

Example:

(who:with-html-output (forms.who:*html*)
   (forms:render-form my-form))")

(defmacro with-form-html-output (&body body)
  `(who:with-html-output (*html* (or *html*
				     (error "FORMS.WHO:*HTML* is unbound. Please bind it before rendering forms. See FORMS.WHO:*HTML* documentation.")))
     ,@body))

(defmethod forms::renderer-render-form ((renderer (eql :who))
                                        (theme forms::default-form-theme)
                                        form &rest args)
  (with-form-html-output
    (apply #'forms::renderer-render-form-errors renderer theme form args)
    (:form :id (forms::form-id form)
           :action (forms::form-action form)
           :method (symbol-name (forms::form-method form))
           :enctype (forms::form-enctype form)
           :class (getf args :class)
           :data-parsley-validate (when (forms::client-validation form)
                                    "true")
           (when (forms::form-csrf-protection-p form)
             (let ((token (forms::set-form-session-csrf-token form)))
               (htm
                (:input :name (forms::form-csrf-field-name form)
                        :type "hidden" :value token))))
           (loop for field in (forms::form-fields form)
              do
                (forms::renderer-render-field renderer theme (cdr field) form)))))

(defmethod forms::renderer-render-form :after ((renderer (eql :who))
                                               (theme forms::default-form-theme)
                                               form &rest args)
  (declare (ignorable args))
  (when (forms::client-validation form)
    (with-html-output (*html*)
      (:script :type "text/javascript"
               (fmt "$('#~A').parsley();" (forms::form-id form))))))

(defmethod forms::renderer-render-form-start ((renderer (eql :who))
                                              (theme forms::default-form-theme)
                                              form &rest args)
  "Start form rendering"
  (fmt:with-fmt ((or *html* (error "FORMS.WHO:*HTML* is unbound. Please bind it before rendering forms. See FORMS.WHO:*HTML* documentation.")))
    "<form id=\"" (forms::form-id form) "\""
    " action=\"" (forms::form-action form) "\""
    " method=\"" (forms::form-method form) "\""
    (:when (forms::form-enctype form)
           " enctype=\"" (forms::form-enctype form) "\"")
    (:when (getf args :class)
           " class=\"" (getf args :class) "\"" )
    (:when (forms::client-validation form)
           " data-parsley-validate")
    ">")
  (when (forms::form-csrf-protection-p form)
    (let ((token (forms::set-form-session-csrf-token form)))
      (format *html* "<input name=\"~A\" type=\"hidden\" value=\"~A\"/>"
              (forms::form-csrf-field-name form)
              token))))

(defmethod forms::renderer-render-form-end ((renderer (eql :who))
                                            (theme forms::default-form-theme)
                                            form)
  "Finish form rendering"
  (format *html* "</form>")
  #+nil(when (forms::client-validation form)
         (with-html-output (*html*)
           (:script :type "text/javascript"
                    (fmt "$('#~A').parsley();" (forms::form-id form))))))

(defmethod forms::renderer-render-form-errors ((renderer (eql :who))
                                               (theme forms::default-form-theme)
                                               form &rest args)
  (when (forms::form-errors form)
    (with-html-output (*html*)
      (:ul :class (or (getf args :class) "errors parsley-errors-list filled")
           (loop for error in (forms::form-errors form)
              do
                (htm (:li (fmt "~A: ~{~A~^, ~}"
                               (or (forms::field-label (first error))
                                   (forms::field-name (first error)))
                               (cdr error)))))))))

(defmethod forms::renderer-render-field ((renderer (eql :who))
                                         (theme forms::default-form-theme)
                                         field form &rest args)
  (with-html-output (*html*)
    (:div :class (when (not (forms:field-valid-p field form))
                   "has-error")
          (apply #'forms::renderer-render-field-label renderer theme field form args)
          (apply #'forms::renderer-render-field-widget renderer theme field form args)
          (apply #'forms::renderer-render-field-errors renderer theme field form args))))

(defmethod forms::renderer-render-field-label ((renderer (eql :who))
                                               (theme forms::default-form-theme)
                                               field form &rest args)
  (when (forms::field-render-label-p field)
    (with-html-output (*html*)
      (:label
       :class (getf args :class)
       (str (or (forms::field-label field)
                (forms::field-name field)))))))

(defmethod forms::renderer-render-field-errors ((renderer (eql :who))
                                                (theme forms::default-form-theme)
                                                field form &rest args)
  (let ((errors (cdr (find field
                           (forms::form-errors form)
                           :key 'car))))
    (when errors
      (with-html-output (*html*)
        (:div :class (or (getf args :class) "errors")
              (fmt "~{~A~^, ~}" errors))))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::string-form-field)
     form &rest args)
  (format *html* "<input type=\"text\"")
  (format *html* " name=\"~A\"" (forms::render-field-request-name field form))
  (when (getf args :class)
    (format *html* " class=\"~A\"" (getf args :class)))
  (when (forms::field-placeholder field)
    (format *html* " placeholder=\"~A\"" (forms::field-placeholder field)))
  (apply #'renderer-render-field-attributes renderer theme field form args)
  (when (forms::field-value field)
    (format *html* " value=\"~A\""
            (who:escape-string (forms:format-field-value-to-string field))))
  (format *html* "></input>"))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::text-form-field)
     form &rest args)
  (format *html* "<textarea")
  (format *html* " name=\"~A\"" (forms::render-field-request-name field form))
  (when (getf args :class)
    (format *html* " class=\"~A\"" (getf args :class)))
  (apply #'renderer-render-field-attributes renderer theme field form args)
  (format *html* ">")
  (when (forms::field-value field)
    (write-string (forms:format-field-value-to-string field) *html*))
  (format *html* "</textarea>"))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::email-form-field) form &rest args)
  (format *html* "<input type=\"email\"")
  (format *html* " name=\"~A\"" (forms::render-field-request-name field form))
  (when (getf args :class)
    (format *html* " class=\"~A\"" (getf args :class)))
  (when (forms::field-placeholder field)
    (format *html* " placeholder=\"~A\"" (forms::field-placeholder field)))
  (apply #'renderer-render-field-attributes renderer theme field form args)
  (when (forms::field-value field)
    (format *html* " value=\"~A\""
            (forms:format-field-value-to-string field)))
  (format *html* "></input>"))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::date-form-field) form &rest args)
  (format *html* "<input type=\"date\"")
  (format *html* " name=\"~A\"" (forms::render-field-request-name field form))
  (when (getf args :class)
    (format *html* " class=\"~A\"" (getf args :class)))
  (when (forms::field-placeholder field)
    (format *html* " placeholder=\"~A\"" (forms::field-placeholder field)))
  (when (forms::date-min field)
    (format *html* " min=\"~A\"" (forms::date-min field)))
  (when (forms::date-max field)
    (format *html* " max=\"~A\"" (forms::date-max field)))
  (apply #'renderer-render-field-attributes renderer theme field form args)
  (when (forms::field-value field)
    (format *html* " value=\"~A\""
            (forms:format-field-value-to-string field)))
  (format *html* "></input>"))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::datetime-form-field) form &rest args)
  (format *html* "<input type=\"datetime\"")
  (format *html* " name=\"~A\"" (forms::render-field-request-name field form))
  (when (getf args :class)
    (format *html* " class=\"~A\"" (getf args :class)))
  (when (forms::field-placeholder field)
    (format *html* " placeholder=\"~A\"" (forms::field-placeholder field)))
  (when (forms::datetime-min field)
    (format *html* " min=\"~A\"" (forms::datetime-min field)))
  (when (forms::datetime-max field)
    (format *html* " max=\"~A\"" (forms::datetime-max field)))
  (apply #'renderer-render-field-attributes renderer theme field form args)
  (when (forms::field-value field)
    (format *html* " value=\"~A\""
            (forms:format-field-value-to-string field)))
  (format *html* "></input>"))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::url-form-field) form &rest args)
  (format *html* "<input type=\"url\"")
  (format *html* " name=\"~A\"" (forms::render-field-request-name field form))
  (when (getf args :class)
    (format *html* " class=\"~A\"" (getf args :class)))
  (when (forms::field-placeholder field)
    (format *html* " placeholder=\"~A\"" (forms::field-placeholder field)))
  (apply #'renderer-render-field-attributes renderer theme field form args)
  (when (forms::field-value field)
    (format *html* " value=\"~A\""
            (forms:format-field-value-to-string field)))
  (format *html* "></input>"))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::integer-form-field) form &rest args)
  (format *html* "<input type=\"number\"")
  (format *html* " name=\"~A\"" (forms::render-field-request-name field form))
  (when (getf args :class)
    (format *html* " class=\"~A\"" (getf args :class)))
  (when (forms::field-placeholder field)
    (format *html* " placeholder=\"~A\"" (forms::field-placeholder field)))
  (apply #'renderer-render-field-attributes renderer theme field form args)
  (when (forms::field-value field)
    (format *html* " value=\"~A\""
            (forms:format-field-value-to-string field)))
  (format *html* "></input>"))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::password-form-field)
     form &rest args)
  (format *html* "<input type=\"password\"")
  (format *html* " name=\"~A\"" (forms::render-field-request-name field form))
  (when (getf args :class)
    (format *html* " class=\"~A\"" (getf args :class)))
  (when (forms::field-placeholder field)
    (format *html* " placeholder=\"~A\"" (forms::field-placeholder field)))
  (apply #'renderer-render-field-attributes renderer theme field form args)
  (when (forms::field-value field)
    (format *html* " value=\"~A\""
            (forms:format-field-value-to-string field)))
  (format *html* "></input>"))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::boolean-form-field) form &rest args)
  (with-html-output (*html*)
    (format *html* "<input type=\"checkbox\"")
    (format *html* " name=\"~A\"" (forms::render-field-request-name field form))
    (when (getf args :class)
      (format *html* " class=\"~A\"" (getf args :class)))
    (when (getf args :id)
      (format *html* " id=\"~A\"" (getf args :id)))
    (apply #'renderer-render-field-attributes renderer theme field form args)
    (when (forms::field-value field)
      (format *html* " checked=\"checked\""))
    (format *html* "></input>")))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::file-form-field) form &rest args)
  (with-html-output (*html*)
    (:input :type "file"
            :class (getf args :class)
            :id (getf args :id)
            :name (forms::render-field-request-name field form)
            :accept (forms::file-accept field))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::hidden-form-field) form &rest args)
  (declare (ignorable args))
  (with-html-output (*html*)
    (:input :type "hidden"
            :name (forms::render-field-request-name field form)
            :value (forms:field-value field))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::submit-form-field) form &rest args)
  (declare (ignorable args))
  (with-html-output (*html*)
    (:input :type "submit"
            :class (getf args :class)
            :value (or (forms::field-label field) "Submit"))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::choice-form-field) form &rest args)
  (declare (ignorable args))
  (cond
    ((and (forms::field-expanded field)
          (forms::field-multiple field))
     (let ((selected-keys (mapcar #'first (forms::field-keys-and-values field))))
       ;; Render checkboxes
       (with-html-output (*html*)
         (loop for (key . choice) in (forms::field-choices-alist field)
            do
              (htm
               (:input :type "checkbox" :name (forms::render-field-request-name field form)
                       :value key
                       :checked (when (member key selected-keys)
                                  "checked")
                       (str (forms:format-field-value-to-string field
                                                                choice))))))))
    ((and (forms::field-expanded field)
          (not (forms::field-multiple field)))
     ;; Render radio buttons
     (let ((selected-value (forms::field-key-and-value field)))
       (with-html-output (*html*)
         (loop for (key . choice) in (forms::field-choices-alist field)
            do
              (htm
               (:input :type "radio" :name (forms::render-field-request-name field form)
                       :value (princ-to-string key)
                       :checked (when (equalp (first selected-value)
                                              key)
                                  "checked")
                       (str (forms:format-field-value-to-string field
                                                                choice))))))))
    ((and (not (forms::field-expanded field))
          (forms::field-multiple field))
     ;; A multiple select box
     (let ((selected-keys (mapcar #'first (forms::field-keys-and-values field))))
       (with-html-output (*html*)
         (:select
          :name (forms::render-field-request-name field form)
          :id (getf args :id)
          :multiple "multiple"
          :attrs (apply #'renderer-render-field-attributes renderer theme field form args)
          (:option :style "display:none;") ;; Empty choice
          (loop for (key . choice) in (forms::field-choices-alist field)
             do
               (htm
                (:option :value (princ-to-string key)
                         :selected (when (member key selected-keys)
                                     "selected")
                         (str (forms:format-field-value-to-string field
                                                                  choice)))))))))
    ((and (not (forms::field-expanded field))
          (not (forms::field-multiple field)))
     ;; A single select box
     (let ((selected-value (forms::field-key-and-value field)))
       (with-html-output (*html*)
         (:select
          :name (forms::render-field-request-name field form)
          :id (getf args :id)
          :attrs (apply #'renderer-render-field-attributes renderer theme field form args)
          (:option :style "display:none;") ;; Empty choice
          (loop for (key . choice) in (forms::field-choices-alist field)
             do
               (htm
                (:option :value (princ-to-string key)
                         :selected (when (equalp (first selected-value)
                                                 key)
                                     "selected")
                         (str (forms:format-field-value-to-string field
                                                                  choice)))))))))))

;; Attributes and constraints
(defmethod renderer-render-field-attributes ((renderer (eql :who))
                                             theme
                                             field form &rest args)
  ;; Field validation attributes
  (when (forms::client-validation form)
    (when (forms::field-required-p field)
      (format *html* " data-parsley-required=\"true\""))
    (when (forms::field-validation-triggers field)
      (fmt:fmt *html* " data-parsley-trigger=\""
               (:join "," (forms::field-validation-triggers field) (:a _ :downcase))
               "\"")))

  (when (forms::field-disabled-p field)
    (format *html* " disabled=\"disabled\""))
  (when (forms::field-read-only-p field)
    (format *html* " readonly=\"readonly\""))

  ;; HTML attributes
  (loop for key in (getf args :attrs) by #'cddr
     for val in (cdr (getf args :attrs)) by #'cddr
     do
       (format *html* " ~A=\"~A\"" key val))
  ;; Constraints
  (loop for constraint in (forms::field-constraints field)
     do (renderer-render-field-constraint renderer constraint field form)))

(defmethod renderer-render-field-attributes ((renderer (eql :who))
                                             theme
                                             (field forms::integer-form-field)
                                             form &rest args)
  (declare (ignorable args))
  (format *html* " data-parsley-type=\"integer\"")
  (call-next-method))

(defmethod renderer-render-field-constraint (renderer constraint field form))
(defmethod renderer-render-field-constraint ((renderer (eql :who))
                                             (constraint clavier:length-validator)
                                             field form)
  (when (clavier::validator-min constraint)
    (format *html* " data-parsley-minlength=\"~A\"" (clavier::validator-min constraint)))
  (when (clavier::validator-max constraint)
    (format *html* " data-parsley-maxlength=\"~A\"" (clavier::validator-max constraint))))

(defmethod renderer-render-field-constraint ((renderer (eql :who))
                                             (constraint clavier:greater-than-validator)
                                             field form)
  (format *html* " data-parsley-min=\"~A\""
          (1+ (clavier::validator-number constraint))))

(defmethod renderer-render-field-constraint ((renderer (eql :who))
                                             (constraint clavier:less-than-validator)
                                             field form)
  (format *html* " data-parsley-max=\"~A\""
          (1- (clavier::validator-number constraint))))

;; Subform field

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::subform-form-field)
     form &rest args)
  (declare (ignore args))
  ;; Render the fields of the subform (but not the HTML form element - form elements inside other form elements is not supported in HTML)
  (let ((subform (or (forms:field-value field)
                     (forms::field-subform field))))
    (loop for field in (forms::form-fields subform)
          do
             (forms::renderer-render-field renderer theme (cdr field)
                                           subform))))

;; List field
(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::list-form-field)
     form &rest args)
  (declare (ignore args))
  (loop for item in (forms::field-value field)
        for i from 0
        do
           (let ((forms::*field-path* (cons (format nil "~a[~a]"
                                                    (string-downcase
                                                     (string (forms::field-name field)))
                                                    i)
                                            (rest forms::*field-path*)))
                 (field (funcall (forms::list-field-type field))))
             (setf (forms:field-value field) item)
             (forms::renderer-render-field-widget renderer theme field form))
        finally
           ;; Render a new entry
           (let ((forms::*field-path* (cons (format nil "~a[~a]"
                                                    (string-downcase
                                                     (string (forms::field-name field)))
                                                    (1+ i))
                                            (rest forms::*field-path*)))
                 (field (funcall (forms::list-field-type field))))
             (forms::renderer-render-field-widget renderer theme field form))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::stateful-file-field) form &rest args)
  (who:with-html-output (forms.who::*html*)
    (when (forms::file-path field)
      (let ((file-info-name (format nil "~A.info"
                                    (forms::render-field-request-name field form))))
        (who:htm
         ;; encode file info to recover it later if no file is uploaded
         (:div
          (:input :type :hidden
                  :name file-info-name
                  :value (base64:string-to-base64-string
                          (prin1-to-string
                           (list :path (forms::file-path field)
                                 :file-name (forms::file-name field)
                                 :content-type (forms::file-content-type field)))))
          (if (forms::download-link field)
              (who:htm (:a :href (forms::download-link field)
                           :target "_blank"
                           (who:str (forms::file-name field))))
              (who:htm (:p (who:str (forms::file-name field)))))
          ;; if the file is not required, render a delete button
          (when (not (forms::field-required-p field))
            (who:htm (:a :href "#" :class "delete-file"
                         :onclick "javascript:$(this).parent().remove();return false;"
                         (who:str "Delete"))))))))
    (:input :type "file"
            :class (getf args :class)
            :accept (forms::file-accept field)
            :name (forms::render-field-request-name field form))))

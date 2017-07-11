(defpackage :cl-forms.who
  (:nicknames :forms.who)
  (:use :cl :forms :who))

(in-package :forms.who)

(defvar *html* nil)

(defmethod forms::call-with-form-renderer ((renderer (eql :who)) function)
  (let ((forms::*form-renderer* renderer))
    (funcall function)))

(defmethod forms::renderer-render-form ((renderer (eql :who))
                                        (theme forms::default-form-theme)
                                        form &rest args)
  (with-html-output (*html*)
    (apply #'forms::renderer-render-form-errors renderer theme form args)
    (:form :id (forms::form-id form)
           :action (forms::form-action form)
           :method (symbol-name (forms::form-method form))
           :class (getf args :class)
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
  (when (forms::client-validation form)
    (with-html-output (*html*)
      (:script :type "text/javascript"
               (fmt "$('#~A').parsley();" (forms::form-id form))))))

(defmethod forms::renderer-render-form-start ((renderer (eql :who))
                                              (theme forms::default-form-theme)
                                              form &rest args)
  "Start form rendering"
  (format *html* "<form id=\"~A\" action=\"~A\" method=\"~A\" ~@[~A~]>"
          (forms::form-id form)
          (forms::form-action form)
          (forms::form-method form)
          (when (getf args :class)
            (format nil " class=\"~A\"" (getf args :class))))
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
  (when (forms::client-validation form)
    (with-html-output (*html*)
      (:script :type "text/javascript"
               (fmt "$('#~A').parsley();" (forms::form-id form))))))

(defmethod forms::renderer-render-form-errors ((renderer (eql :who))
                                               (theme forms::default-form-theme)
                                               form &rest args)
  (when (forms::form-errors form)
    (with-html-output (*html*)
      (:ul :class (or (getf args :class "errors parsley-errors-list filled"))
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
  (unless (typep field 'forms::submit-form-field)
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
  (format *html* " name=\"~A\"" (forms::field-request-name field form))
  (when (getf args :class)
    (format *html* " class=\"~A\"" (getf args :class)))
  (when (forms::field-empty-value field)
    (format *html* " placeholder=\"~A\"" (forms::field-empty-value field)))
  (renderer-render-field-attributes renderer theme field form)
  (when (forms::field-value field)
    (format *html* " value=\"~A\""
            (funcall (forms::field-formatter field)
                     (forms::field-value field))))
  (format *html* "></input>"))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::email-form-field) form &rest args)
  (format *html* "<input type=\"email\"")
  (format *html* " name=\"~A\"" (forms::field-request-name field form))
  (when (getf args :class)
    (format *html* " class=\"~A\"" (getf args :class)))
  (when (forms::field-empty-value field)
    (format *html* " placeholder=\"~A\"" (forms::field-empty-value field)))
  (renderer-render-field-attributes renderer theme field form)
  (when (forms::field-value field)
    (format *html* " value=\"~A\""
            (funcall (forms::field-formatter field)
                     (forms::field-value field))))
  (format *html* "></input>"))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::url-form-field) form &rest args)
  (format *html* "<input type=\"url\"")
  (format *html* " name=\"~A\"" (forms::field-request-name field form))
  (when (getf args :class)
    (format *html* " class=\"~A\"" (getf args :class)))
  (when (forms::field-empty-value field)
    (format *html* " placeholder=\"~A\"" (forms::field-empty-value field)))
  (renderer-render-field-attributes renderer theme field form)
  (when (forms::field-value field)
    (format *html* " value=\"~A\""
            (funcall (forms::field-formatter field)
                     (forms::field-value field))))
  (format *html* "></input>"))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::integer-form-field) form &rest args)
  (format *html* "<input type=\"number\"")
  (format *html* " name=\"~A\"" (forms::field-request-name field form))
  (when (getf args :class)
    (format *html* " class=\"~A\"" (getf args :class)))
  (when (forms::field-empty-value field)
    (format *html* " placeholder=\"~A\"" (forms::field-empty-value field)))
  (renderer-render-field-attributes renderer theme field form)
  (when (forms::field-value field)
    (format *html* " value=\"~A\""
            (funcall (forms::field-formatter field)
                     (forms::field-value field))))
  (format *html* "></input>"))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::password-form-field)
     form &rest args)
  (format *html* "<input type=\"password\"")
  (format *html* " name=\"~A\"" (forms::field-request-name field form))
  (when (getf args :class)
    (format *html* " class=\"~A\"" (getf args :class)))
  (when (forms::field-empty-value field)
    (format *html* " placeholder=\"~A\"" (forms::field-empty-value field)))
  (renderer-render-field-attributes renderer theme field form)
  (when (forms::field-value field)
    (format *html* " value=\"~A\""
            (funcall (forms::field-formatter field)
                     (forms::field-value field))))
  (format *html* "></input>"))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::boolean-form-field) form &rest args)
  (with-html-output (*html*)
    (:input :type "checkbox"
            :class (getf args :class)
            :name (forms::field-request-name field form)
            :checked (when (forms::field-value field)
                       "checked"))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::submit-form-field) form &rest args)
  (with-html-output (*html*)
    (:input :type "submit"
            :class (getf args :class)
            :value (or (forms::field-label field) "Submit"))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (theme forms::default-form-theme)
     (field forms::choice-form-field) form &rest args)
  (cond
    ((and (forms::field-expanded field)
          (forms::field-multiple field))
     (let ((selected-keys (mapcar #'first (forms::field-keys-and-values field))))
       ;; Render checkboxes
       (with-html-output (*html*)
         (loop for (key . choice) in (forms::field-choices-alist field)
            do
              (htm
               (:input :type "checkbox" :name (forms::field-request-name field form)
                       :value key
                       :checked (when (member key selected-keys)
                                  "checked")
                       (str (funcall (forms::field-formatter field)
                                     choice))))))))
    ((and (forms::field-expanded field)
          (not (forms::field-multiple field)))
     ;; Render radio buttons
     (let ((selected-value (forms::field-key-and-value field)))
       (with-html-output (*html*)
         (loop for (key . choice) in (forms::field-choices-alist field)
            do
              (htm
               (:input :type "radio" :name (forms::field-request-name field form)
                       :value (princ-to-string key)
                       :checked (when (equalp (first selected-value)
                                              key)
                                  "checked")
                       (str (funcall (forms::field-formatter field)
                                     choice))))))))
    ((and (not (forms::field-expanded field))
          (forms::field-multiple field))
     ;; A multiple select box
     (let ((selected-keys (mapcar #'first (forms::field-keys-and-values field))))
       (with-html-output (*html*)
         (:select
          :name (forms::field-request-name field form)
          :multiple "multiple"
          (loop for (key . choice) in (forms::field-choices-alist field)
             do
               (htm
                (:option :value (princ-to-string key)
                         :selected (when (member key selected-keys)
                                     "selected")
                         (str (funcall (forms::field-formatter field)
                                       choice)))))))))
    ((and (not (forms::field-expanded field))
          (not (forms::field-multiple field)))
     ;; A single select box
     (let ((selected-value (forms::field-key-and-value field)))
       (with-html-output (*html*)
         (:select
          :name (forms::field-request-name field form)
          (loop for (key . choice) in (forms::field-choices-alist field)
             do
               (htm
                (:option :value (princ-to-string key)
                         :selected (when (equalp (first selected-value)
                                                 key)
                                     "selected")
                         (str (funcall (forms::field-formatter field)
                                       choice)))))))))))

;; Attributes and constraints
(defmethod renderer-render-field-attributes ((renderer (eql :who))
                                             theme
                                             field form)
  (when (forms::client-validation form)
    (when (forms::field-required-p field)
      (format *html* " data-parsley-required=\"true\""))
    (loop for constraint in (forms::field-constraints field)
       do (renderer-render-field-constraint renderer constraint field form))))

(defmethod renderer-render-field-attributes ((renderer (eql :who))
                                             theme
                                             (field forms::integer-form-field)
                                             form)
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
  (let ((forms::*field-path* (cons "." forms::*field-path*)))
    ;; Render the fields of the subform (but not the HTML form element - form elements inside other form elements is not supported in HTML)
    (let ((subform (or (forms:field-value field)
                       (forms::field-subform field))))
      (loop for field in (forms::form-fields subform)
         do
           (forms::renderer-render-field renderer theme (cdr field)
                                         subform)))))

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
       (let ((forms::*field-path* (cons (list "[" i "].") forms::*field-path*)))
         (forms::renderer-render-field-widget renderer theme item form))
     finally
     ;; Render a new entry
       (let ((forms::*field-path* (cons (list "[" (1+ i) "].") forms::*field-path*)))
         (let ((entry (funcall (forms::list-field-type field))))
           (forms::renderer-render-field-widget renderer theme entry form)))))

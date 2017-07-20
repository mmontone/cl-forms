(defpackage #:cl-forms
  (:nicknames :forms)
  (:use #:cl)
  (:export :with-form
           :with-form-renderer
           :with-form-theme
           :defform
           :defform-builder
           :get-form
           :get-field
           :get-field-value
           :render-form
           :render-form-start
           :render-form-end
           :render-field
           :render-field-label
           :render-field-errors
           :render-field-widget
           :render-form-errors
           :with-form-fields
           :with-form-field-values
           :field-value
           :field-valid-p
           :field-formatter
           :field-parser
           :handle-request
           :validate-form
           :form-valid-p
           :format-field-value
           :format-field-value-to-string
           :*base64-encode*))

(defpackage #:cl-forms
  (:nicknames :forms)
  (:use #:cl)
  (:export :form-field
           :with-form
           :with-form-renderer
           :with-form-theme
           :defform
           :defform-builder
           :get-form
           :get-field
           :get-field-value
           :set-field-value
           :render-form
           :render-form-start
           :render-form-end
           :render-field
           :render-field-label
           :render-field-errors
           :render-field-widget
           :render-form-errors
           :fill-form-from-model
           :fill-model-from-form
           :with-form-fields
           :with-form-field-values
           :field-value
           :field-valid-p
           :field-formatter
           :field-parser
           :field-accessor
           :field-writer
           :field-reader
           :add-field
           :remove-field
           :handle-request
           :validate-form
           :form-valid-p
           :form-errors
           :add-form-error
           :with-form-template
           :format-field-value
           :format-field-value-to-string
           :make-formatter
           :*base64-encode*))

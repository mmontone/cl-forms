(in-package :forms.test)

(hunchentoot:define-easy-handler (demo-renderers :uri "/renderers") ()
  (flet ((render ()
           (forms:with-form-renderer :who
	     (who:with-html-output (forms.who:*html*)
	       (:h2 "CL-WHO automatic")
	       (:p (who:str "Render via CL-WHO and whole form with RENDER-FORM."))
	       (forms:render-form (forms:get-form 'fields-form))
	       (:h2 "CL-WHO manual")
	       (:p (who:str "Render via CL-WHO and the individual rendering functions RENDER-FORM-START, RENDER-FORM-END, RENDER-FIELD, RENDER-FIELD-LABEL and RENDER-FIELD-WIDGET."))
	       (forms:with-form (forms:get-form 'fields-form)
		 (forms:render-form-start)
		 (forms:render-field 'name)
		 (forms:render-field-label 'ready)
		 (forms:render-field-widget 'ready)
		 (forms:render-field 'sex)
		 (forms:render-field 'avatar)
		 (forms:render-field 'disabled)
		 (forms:render-field 'readonly)
		 (forms:render-field 'readonly-checkbox)
		 (forms:render-field 'disabled-checkbox)
		 (forms:render-field 'submit)
		 (forms:render-form-end))
               ))))
    (render-demo-page :demo #'render
                      :source (asdf:system-relative-pathname :cl-forms.demo
                                                             "test/demo/renderers.lisp")
                      :active-menu :renderers)))



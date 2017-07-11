(defpackage :cl-forms.qimt
  (:nicknames :forms.qimt)
  (:use :cl :forms :qimt :html))

(in-package :forms.qimt)

(qimt::%define-attribute :forms.qimt <placeholder= "placeholder")

(defmethod forms::renderer-render-form ((renderer (eql :qimt))
					(theme forms::default-form-theme)
					form &rest args)
  (when (forms::form-errors form)
    (<ul
      (loop for error in (forms::form-errors form)
	 do
	   (<li (fxd "~A: ~{~A~^, ~}" (first error) (cdr error))))))
  (<form (<action= (forms::form-action form))
	 (<method= (symbol-name (forms::form-method form)))
	 (when (forms::form-csrf-protection-p form)
	   (let ((token (forms::set-form-session-csrf-token form)))
	     (<input (<name= (forms::form-csrf-field-name form))
		     (<type= "hidden")
		     (<value= token))))
	 (loop for field in (forms::form-fields form)
	    do
	      (forms::renderer-render-field renderer theme (cdr field) form))))

(defmethod forms::renderer-render-form :after ((renderer (eql :qimt))
					       (theme forms::default-form-theme)
					       form &rest args)
  (when (forms::client-validation form)
    (<script (<type= "text/javascript")
	     (fxd "$('#~A').parsley();" (forms::form-id form)))))

(defmethod forms::renderer-render-form-errors ((renderer (eql :qimt))
					       (theme forms::default-form-theme)
					       form &rest args)
  (when (forms::form-errors form)
    (<ul (<class= "errors")
	 (loop for error in (forms::form-errors form)
	    do
	      (<li (fxd "~A: ~{~A~^, ~}" (first error) (cdr error)))))))

(defmethod forms::renderer-render-field ((renderer (eql :qimt))
					 (theme forms::default-form-theme)
					 field form &rest args)
  (<div
    (forms::renderer-render-field-label renderer theme field form)
    (forms::renderer-render-field-errors renderer theme field form)
    (forms::renderer-render-field-widget renderer theme field form)))

(defmethod forms::renderer-render-field-label ((renderer (eql :qimt))
					       (theme forms::default-form-theme)
					       field form &rest args)
  (<label
    (xd (or (forms::field-label field)
	    (forms::field-name field)))))

(defmethod forms::renderer-render-field-label ((renderer (eql :qimt))
					       (theme forms::default-form-theme)
					       (field forms::submit-form-field) form &rest args)
  )

(defmethod forms::renderer-render-field-errors ((renderer (eql :qimt))
						(theme forms::default-form-theme)
						field form &rest args)
  (let ((errors (cdr (assoc (forms::field-name field)
                            (forms::form-errors form)
                            :test #'equalp
                            :key #'string))))
    (when errors
      (<div (<class= "errors")
	    (fxd "~{~A~^, ~}" errors)))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :qimt))
     (theme forms::default-form-theme)
     (field forms::string-form-field) form &rest args)
  (<input (<type= "text")
	  (<name= (forms::render-field-request-name field form))
	  (when (forms::field-empty-value field)
	    (<placeholder= (forms::field-empty-value field)))
	  (renderer-render-field-attributes renderer theme field form)
	  (when (forms::field-value field)
	    (<value=
	     (funcall (forms::field-formatter field)
		      (forms::field-value field))))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :qimt))
     (theme forms::default-form-theme)
     (field forms::email-form-field) form &rest args)
  (<input (<type= "email")
	  (<name= (forms::render-field-request-name field form))
	  (when (forms::field-empty-value field)
	    (<placeholder= (forms::field-empty-value field)))
	  (renderer-render-field-attributes renderer theme field form)
	  (when (forms::field-value field)
	    (<value=
	     (funcall (forms::field-formatter field)
		      (forms::field-value field))))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :qimt))
     (theme forms::default-form-theme)
     (field forms::url-form-field) form &rest args)
  (<input (<type= "url")
	  (<name= (forms::render-field-request-name field form))
	  (when (forms::field-empty-value field)
		 (<placeholder= (forms::field-empty-value field)))
	  (renderer-render-field-attributes renderer theme field form)
	  (when (forms::field-value field)
	    (<value=
	     (funcall (forms::field-formatter field)
		      (forms::field-value field))))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :qimt))
     (theme forms::default-form-theme)
     (field forms::integer-form-field) form &rest args)
  (<input (<type= "number")
	  (<name= (forms::render-field-request-name field form))
	  (when (forms::field-empty-value field)
	    (<placeholder= (forms::field-empty-value field)))
	  (renderer-render-field-attributes renderer theme field form)
	  (when (forms::field-value field)
	    (<value=
	     (funcall (forms::field-formatter field)
		      (forms::field-value field))))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :qimt))
     (theme forms::default-form-theme)
     (field forms::password-form-field) form &rest args)
  (<input (<type= "password")
	  (<name= (forms::render-field-request-name field form))
	  (when (forms::field-empty-value field)
	    (<placeholder= (forms::field-empty-value field)))
	  (renderer-render-field-attributes renderer theme field form)
	  (when (forms::field-value field)
	    (<value=
	     (funcall (forms::field-formatter field)
		      (forms::field-value field))))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :qimt))
     (theme forms::default-form-theme)
     (field forms::boolean-form-field) form &rest args)
  (<input (<type= "checkbox")
	  (<name= (forms::render-field-request-name field form))
	  (when (forms::field-value field)
	    (<checked= "checked"))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :qimt))
     (theme forms::default-form-theme)
     (field forms::submit-form-field) form &rest args)
  (<input (<type= "submit")
	  (<value= (or (forms::field-label field) "Submit"))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :qimt))
     (theme forms::default-form-theme)
     (field forms::choice-form-field) form &rest args)
  (cond
    ((and (forms::field-expanded field)
	  (forms::field-multiple field))
     (let ((selected-keys (mapcar #'first (forms::field-keys-and-values field))))
       ;; Render checkboxes
       (loop for (key . choice) in (forms::field-choices-alist field)
	  do
	    (<input (<type= "checkbox")
		    (<name= (forms::render-field-request-name field form))
		    (<value= key)
		    (when (member key selected-keys)
		      (<checked= "checked"))
		    (xd (funcall (forms::field-formatter field)
				 choice))))))
    ((and (forms::field-expanded field)
	  (not (forms::field-multiple field)))
     ;; Render radio buttons
     (let ((selected-value (forms::field-key-and-value field)))
       (loop for (key . choice) in (forms::field-choices-alist field)
	  do
	    (<input (<type= "radio")
		    (<name= (forms::render-field-request-name field form))
		    (<value= (princ-to-string key))
		    (when (equalp (first selected-value)
				  key)
		      (<checked= "checked"))
		    (xd (funcall (forms::field-formatter field)
				 choice))))))
    ((and (not (forms::field-expanded field))
	  (forms::field-multiple field))
     ;; A multiple select box
     (let ((selected-keys (mapcar #'first (forms::field-keys-and-values field))))
       (<select
	 (<name= (forms::render-field-request-name field form))
	 (<multiple= "multiple")
	 (loop for (key . choice) in (forms::field-choices-alist field)
	    do
	      (<option (<value= (princ-to-string key))
		       (when (member key selected-keys)
			 (<selected= "selected"))
		       (xd (funcall (forms::field-formatter field)
				    choice)))))))
    ((and (not (forms::field-expanded field))
	  (not (forms::field-multiple field)))
     ;; A single select box
     (let ((selected-value (forms::field-key-and-value field)))
       (<select
	 (<name= (forms::render-field-request-name field form))
	 (loop for (key . choice) in (forms::field-choices-alist field)
	    do
	      (<option (<value= (princ-to-string key))
		       (when (equalp (first selected-value)
				     key)
			 (<selected= "selected"))
		       (xd (funcall (forms::field-formatter field)
				    choice)))))))))

;; Attributes and constraints
(qimt::%define-attribute :forms.qimt <data-parsley-required= "data-parsley-required")
(qimt::%define-attribute :forms.qimt <data-parsley-type= "data-parsley-type")
(qimt::%define-attribute :forms.qimt <data-parsley-minlength= "data-parsley-minlength")
(qimt::%define-attribute :forms.qimt <data-parsley-maxlength= "data-parsley-maxlength")

(defmethod renderer-render-field-attributes ((renderer (eql :qimt))
					     theme
                                             field form)
  (when (forms::client-validation form)
    (when (forms::field-required-p field)
      (<data-parsley-required= "true"))
    (loop for constraint in (forms::field-constraints field)
       do (renderer-render-field-constraint renderer constraint field form))))

(defmethod renderer-render-field-attributes ((renderer (eql :qimt))
					     theme
					     (field forms::integer-form-field)
					     form)
  (<data-parsley-type= "integer")
  (call-next-method))

(defmethod renderer-render-field-constraint (renderer constraint field form))
(defmethod renderer-render-field-constraint ((renderer (eql :qimt))
                                             (constraint clavier:length-validator)
                                             field form)
  (when (clavier::validator-min constraint)
    (<data-parsley-minlength= (clavier::validator-min constraint)))
  (when (clavier::validator-max constraint)
    (<data-parsley-maxlength= (clavier::validator-max constraint))))

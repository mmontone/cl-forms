(in-package :forms)

(defclass string-form-field (form-field)
  ()
  (:documentation "A string input field"))

(defclass text-form-field (string-form-field)
  ()
  (:documentation "A text field. Renders as a text area"))

(defmethod validate-form-field ((form-field string-form-field))
  (multiple-value-bind (valid-p error)
   (funcall (clavier:is-a-string "~A should be a string" (field-name form-field))
	    (field-value form-field))
    (multiple-value-bind (valid-constraints-p errors)
	(call-next-method)
      (values (and valid-p valid-constraints-p)
	      (if error (cons error errors)
		  errors)))))

(defmethod field-read-from-request ((field string-form-field) form parameters)
  (setf (field-value field)
	(cdr (assoc (field-request-name field form) parameters :test #'string=))))

(defmethod make-form-field ((field-type (eql :string)) &rest args)
  (apply #'make-instance 'string-form-field args))

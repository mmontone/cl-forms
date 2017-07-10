(in-package :forms)

(defclass boolean-form-field (form-field)
  ()
  (:documentation "A boolean input"))

(defmethod validate-form-field ((form-field boolean-form-field))
  (multiple-value-bind (valid-p error)
      (funcall (clavier:is-a-boolean "~A should be a boolean" (field-name form-field))
	       (field-value form-field))
    (multiple-value-bind (valid-constraints-p errors)
	(call-next-method)
      (values (and valid-p valid-constraints-p)
	      (if error (cons error errors)
		  errors)))))

(defmethod field-read-from-request ((field boolean-form-field) form parameters)
  (setf (field-value field)
	(equalp 
	 (cdr (assoc (field-request-name field form) parameters :test #'string=)) 
	 "on")))

(defmethod make-form-field ((field-type (eql :boolean)) &rest args)
  (apply #'make-instance 'boolean-form-field args))

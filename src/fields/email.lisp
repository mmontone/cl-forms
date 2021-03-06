(in-package :forms)

(defclass email-form-field (form-field)
  ()
  (:documentation "A string input field"))

(defmethod validate-form-field ((form-field email-form-field))
  (multiple-value-bind (valid-p error)
      (funcall (clavier:valid-email (or (field-invalid-message form-field)
                                        "The email is not valid"))
               (field-value form-field))
    (multiple-value-bind (valid-constraints-p errors)
        (call-next-method)
      (values (and valid-p valid-constraints-p)
              (if error (cons error errors)
                  errors)))))

(defmethod field-read-from-request ((field email-form-field) form parameters)
  (setf (field-value field)
        (cdr (assoc (field-request-name field form) parameters :test #'string=))))

(defmethod make-form-field ((field-type (eql :email)) &rest args)
  (apply #'make-instance 'email-form-field args))

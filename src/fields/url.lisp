(in-package :forms)

(defclass url-form-field (form-field)
  ()
  (:documentation "An url input field"))

(defmethod validate-form-field ((form-field url-form-field))
  (and
   (funcall (clavier:valid-url "~A is not a valid url"
                               (or (field-label form-field)
                                   (field-name form-field)))
            (field-value form-field))
   (call-next-method)))

(defmethod field-read-from-request ((field url-form-field) form parameters)
  (setf (field-value field)
        (cdr (assoc (field-request-name field form) parameters :test #'string=))))

(defmethod make-form-field ((field-type (eql :url)) &rest args)
  (apply #'make-instance 'url-form-field args))

(in-package :forms)

(defclass boolean-form-field (form-field)
  ()
  (:documentation "A boolean input"))

(defmethod field-required-p ((form-field boolean-form-field))
  nil)

(defmethod validate-form-field ((form-field boolean-form-field))
  (multiple-value-bind (valid-p error)
      (funcall (clavier:is-a-boolean (or (field-invalid-message form-field)
                                         "~A should be a boolean")
                                     (or (field-label form-field)
                                         (field-name form-field)))
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

(defmethod format-field-value ((field boolean-form-field) value &optional (stream *standard-output*))
  (if value
      (write-string "on" stream)
      (write-string "off" stream)))

(defmethod make-form-field ((field-type (eql 'boolean)) &rest args)
  (apply #'make-instance 'boolean-form-field args))

(defmethod make-form-field ((field-type (eql :boolean)) &rest args)
  (apply #'make-instance 'boolean-form-field args))

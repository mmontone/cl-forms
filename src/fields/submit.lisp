(in-package :forms)

(defclass submit-form-field (form-field)
  ()
  (:default-initargs :required-p nil)
  (:documentation "A submit input button"))

(defmethod field-read-from-request ((field submit-form-field) form parameters)
  )

(defmethod make-form-field ((field-type (eql :submit)) &rest args)
  (apply #'make-instance 'submit-form-field args))

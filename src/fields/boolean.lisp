(in-package :forms)

(defclass boolean-form-field (form-field)
  ()
  (:default-initargs :validator (clavier:is-a-boolean))
  (:documentation "A boolean input"))

(defmethod field-read-from-request ((field boolean-form-field) form)
  (setf (field-value field)
	(equalp (hunchentoot:post-parameter (form-field-name field form)) "on")))

(defmethod make-form-field ((field-type (eql :boolean)) &rest args)
  (apply #'make-instance 'boolean-form-field args))

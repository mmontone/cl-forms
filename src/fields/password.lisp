(in-package :forms)

(defclass password-form-field (form-field)
  ()
  (:documentation "A password input field"))

(defmethod field-read-from-request ((field password-form-field) form)
  (setf (field-value field)
	(hunchentoot:post-parameter (form-field-name field form))))

(defmethod make-form-field ((field-type (eql :password)) &rest args)
  (apply #'make-instance 'password-form-field args))

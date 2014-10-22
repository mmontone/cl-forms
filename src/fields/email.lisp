(in-package :forms)

(defclass email-form-field (form-field)
  ()
  (:documentation "A string input field"))

(defmethod validate-form-field ((form-field email-form-field))
  (and
   (funcall (clavier:valid-email "~A is not a valid email" (field-name form-field))
	    (field-value form-field))
   (call-next-method)))

(defmethod field-read-from-request ((field email-form-field) form)
  (setf (field-value field)
	(hunchentoot:post-parameter (form-field-name field form))))

(defmethod make-form-field ((field-type (eql :email)) &rest args)
  (apply #'make-instance 'email-form-field args))

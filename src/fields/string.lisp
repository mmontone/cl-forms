(in-package :forms)

(defclass string-form-field (form-field)
  ()
  (:default-initargs :validator (clavier:is-a-string))
  (:documentation "A string input field"))

(defclass text-form-field (string-form-field)
  ()
  (:default-initargs :validator (clavier:is-a-string))
  (:documentation "A text field. Renders as a text area"))

(defmethod validate-form-field ((form-field string-form-field))
  (and
   (funcall (clavier:is-a-string "~A should be a string" (field-name form-field))
	    (field-value form-field))
   (call-next-method)))

(defmethod field-read-from-request ((field string-form-field) form)
  (setf (field-value field)
	(hunchentoot:post-parameter (form-field-name field form))))

(defmethod make-form-field ((field-type (eql :string)) &rest args)
  (apply #'make-instance 'string-form-field args))


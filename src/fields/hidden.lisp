(in-package :forms)

(defclass hidden-form-field (form-field)
  ()
  (:documentation "A hidden form field"))

(defmethod field-read-from-request ((field hidden-form-field) form parameters)
  (setf (field-value field)
        (cdr (assoc (field-request-name field form) parameters :test #'string=))))

(defmethod make-form-field ((field-type (eql :hidden)) &rest args)
  (apply #'make-instance 'hidden-form-field args))

(defmethod field-render-label-p ((field hidden-form-field))
  nil)

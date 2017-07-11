(in-package :cl-forms)

(defclass subform-form-field (form-field)
  ((subform-builder :initarg :subform-builder
                    :initform nil
                    :type (or null function)
                    :accessor field-subform-builder)
   (subform :initarg :subform
            :initform nil
            :type (or null symbol)
            :accessor field-subform-name))
  (:documentation "A field that contains a form (subform)"))

(defmethod subform-builder ((field subform-form-field))
  (or (field-subform-builder field)
      (get (field-subform-name field) :form)))

(defmethod field-subform ((field subform-form-field))
  (funcall (subform-builder field)))

(defmethod make-form-field ((field-type (eql :subform)) &rest args)
  (apply #'make-instance 'subform-form-field args))

(defmethod field-read-from-request ((field subform-form-field) form parameters)
  (let ((field-subform (field-subform field)))
    (forms::dflet ((forms::field-request-name (fld subform)
                                              (fmt:fmt nil
                                                       (:a (forms::form-name form)) "."                                                  (:a (forms::field-name field)) "."
                                                       (:a (forms::call-next-function)))))
      (handle-request field-subform)
      (setf (field-value field) field-subform))))

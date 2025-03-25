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
      (get (field-subform-name field) :form)
      (error "Cannot build the subform in ~s" field)))

(defmethod field-subform ((field subform-form-field))
  (funcall (subform-builder field)))

(defmethod make-form-field ((field-type (eql :subform)) &rest args)
  (apply #'make-instance 'subform-form-field args))

(defmethod field-read-from-request ((field subform-form-field) form parameters)
  (let ((field-subform (field-subform field)))
    (loop for field in (form-fields field-subform)
          do (field-read-from-request (cdr field) form
                                      parameters))
      (setf (field-value field) field-subform)))

(defmethod field-add-to-path ((form-field subform-form-field) form &optional (path *field-path*))
  (cons (string-downcase (string (field-name form-field)))
         path))

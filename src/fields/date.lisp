(in-package :forms)

(defparameter +default-date-format+
  local-time:+iso-8601-date-format+)

(defclass date-form-field (form-field)
  ((date-min :initarg :date-min
             :accessor date-min
             :initform nil)
   (date-max :initarg :date-max
             :accessor date-max
             :initform nil)
   (date-format :initarg :date-format
                :accessor date-format
                :initform +default-date-format+)
   (widget :initarg :widget
           :accessor field-widget
           :initform nil))
  (:default-initargs
   :placeholder "YYYY-MM-DD")
  (:documentation "A date input field"))

(defclass date-validator (clavier:validator)
  ((date-format :initarg :date-format
                :accessor date-format
                :initform +default-date-format+))
  (:metaclass c2mop:funcallable-standard-class)
  (:default-initargs
   :message (lambda (validator object)
              (declare (ignorable validator object))
              (format nil "The date is invalid: ~A" object)))
  (:documentation "A date validator. TODO: should perhaps be part of clavier validators"))

(defmethod clavier::%validate ((validator date-validator) object &rest args)
  (declare (ignore args))
  (and
   (local-time:parse-timestring object
                                :fail-on-error nil
                                :date-separator #\-
                                :allow-missing-time-part t)
   t))

;;(clavier:validate (make-instance 'date-validator) "2003-10-10")

(defmethod validate-form-field ((form-field date-form-field))
  (let ((validator (clavier:fn (lambda (date)
                                 (typep date 'local-time:timestamp))
                               (format nil "~A is not a valid date"
                                       (field-name form-field)))))
    (multiple-value-bind (valid-p error)
        (funcall validator
                 (field-value form-field))
      (multiple-value-bind (valid-constraints-p errors)
          (call-next-method)
        (values (and valid-p valid-constraints-p)
                (if error (cons error errors)
                    errors))))))

(defmethod field-read-from-request ((field date-form-field) form parameters)
  (let ((value (cdr (assoc (field-request-name field form) parameters :test #'string=))))
    (setf (field-value field)
          (or
           (local-time:parse-timestring
            value
            :date-separator #\-
            :allow-missing-time-part t
            :fail-on-error nil)
           value))))

(defmethod make-form-field ((field-type (eql :date)) &rest args)
  (apply #'make-instance 'date-form-field args))

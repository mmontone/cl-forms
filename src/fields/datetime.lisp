(in-package :forms)

(defparameter +default-datetime-format+
  '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\T (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2)))

(defclass datetime-form-field (form-field)
  ((datetime-min :initarg :min
                 :accessor datetime-min
                 :initform nil)
   (datetime-max :initarg :max
             :accessor datetime-max
             :initform nil)
   (datetime-format :initarg :datetime-format
                :accessor datetime-format
                :initform +default-datetime-format+)
   (widget :initarg :widget
           :accessor field-widget
           :initform nil))
  (:default-initargs
   :placeholder "YYYY-MM-DDTHH:MM:SS")
  (:documentation "A date input field"))

(defclass datetime-validator (clavier:validator)
  ((datetime-format :initarg :datetime-format
                :accessor datetime-format
                :initform +default-datetime-format+))
  (:metaclass c2mop:funcallable-standard-class)
  (:default-initargs
   :message (lambda (validator object)
              (declare (ignorable validator object))
              "The datetime is invalid"))
  (:documentation "A datetime validator. TODO: should perhaps be part of clavier validators"))

(defmethod clavier::%validate ((validator datetime-validator) object &rest args)
  (declare (ignore args))
  (and
   (local-time:parse-timestring
    object
    :fail-on-error nil
    :date-separator #\-
    :allow-missing-time-part nil)
   t))

;;(clavier:validate (make-instance 'date-validator) "2003-10-10")

(defmethod validate-form-field ((form-field datetime-form-field))
  (let ((validator (clavier:fn (lambda (datetime)
                                 (typep datetime 'local-time:timestamp))
                               (or (field-invalid-message form-field)
                                   "The datetime is invalid"))))
    (multiple-value-bind (valid-p error)
        (funcall validator
                 (field-value form-field))
      (multiple-value-bind (valid-constraints-p errors)
          (call-next-method)
        (values (and valid-p valid-constraints-p)
                (if error (cons error errors)
                    errors))))))

(defmethod field-read-from-request ((field datetime-form-field) form parameters)
  (let ((value (cdr (assoc (field-request-name field form) parameters :test #'string=))))
    (setf (field-value field)
          (or
           (and value
                (local-time:parse-timestring
                 value
                 :date-separator #\-
                 :allow-missing-time-part nil
                 :fail-on-error nil))
           (if (string= value "")
               nil
               value)))))

(defmethod format-field-value ((field datetime-form-field) value &optional (stream *standard-output*))
  (if (typep value 'local-time:timestamp)
      (local-time:format-timestring stream value :format (datetime-format field))
      (call-next-method)))

(defmethod make-form-field ((field-type (eql :datetime)) &rest args)
  (apply #'make-instance 'datetime-form-field args))

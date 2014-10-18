;;;; cl-forms.lisp

(in-package #:cl-forms)

(defvar *form-renderer* nil)
(defvar *default-form-renderer* :who)
(defvar *form* nil "The current form")

(defmethod call-with-form-renderer (renderer function)
  (let ((*form-renderer* renderer))
    (funcall function)))

(defmacro with-form-renderer (renderer &body body)
  `(call-with-form-renderer ,renderer (lambda () ,@body)))

(defun call-with-form (form function)
  (let ((*form* form))
    (funcall function)))

(defmacro with-form (form &body body)
  `(call-with-form ,form (lambda () ,@body)))

(defmacro defform (form-name args fields)
  `(setf (get ',form-name :form)
	 (lambda ()
	   (make-instance 'form
			  :name ',form-name
			  :fields (list ,@(loop for field in fields
					     collect
					       (destructuring-bind (field-name field-type &rest field-args) field
						 `(cons ',field-name (make-form-field ,field-type :name ,(string field-name) ,@field-args)))))
			  ,@args))))

(defmacro defform-builder (form-name args &body body)
  (alexandria:with-unique-names (form)
    `(setf (get ',form-name :form)
	   (lambda ,args
	     (let ((,form (progn ,@body)))
	       (setf (form-name ,form) ',form-name)
	       ,form)))))

(defun make-form (name fields &rest options)
  (apply #'make-instance
	 'form
	 :name name
	 :fields (make-form-fields fields)
	 options))

(defun make-form-fields (fields)
  (loop for field in fields
     collect
       (destructuring-bind (field-name field-type &rest field-args) field
	 (cons field-name (apply #'make-form-field field-type :name (string field-name) field-args)))))

(defun get-form (name &rest args)
  (apply (get name :form) args))

(defmacro with-form-fields (fields form &body body)
  `(let ,(loop for field in fields
	      collect `(,field (get-field ,form ',field)))
     ,@body))

(defmacro with-form-field-values (fields form &body body)
  `(let ,(loop for field in fields
	      collect `(,field (field-value (get-field ,form ',field))))
     ,@body))

(defun get-field (form field-name)
  (cdr (assoc field-name (form-fields form))))

(defclass form ()
  ((name :initarg :name
	 :accessor form-name
	 :initform (error "Provide a name for the form")
	 :documentation "The form name")
   (action :initarg :action
	   :initform nil
	   :accessor form-action
	   :documentation "The form action")
   (method :initarg :method
	   :initform :post
	   :accessor form-method
	   :documentation "The form method")
   (fields :initarg :fields
	   :initform nil
	   :accessor form-fields
	   :documentation "Form fields")
   (model :initarg :model
	  :initform nil
	  :accessor form-model
	  :documentation "The form model object")
   (csrf-protection :initarg :csrf-protection-p
		    :initform t
		    :accessor form-csrf-protection-p
		    :documentation "T when csrf protection is enabled")
   (csrf-field-name :initarg :csrf-field-name
		    :initform "_token"
		    :accessor form-csrf-field-name
		    :documentation "csrf field name")
   (errors :initform nil
	   :accessor form-errors
	   :documentation "Form errors after validation"))	    
  (:documentation "A form"))

(defmethod print-object ((form form) stream)
  (print-unreadable-object (form stream :type t :identity t)
    (format stream "~A~@{ ~A~}"
	    (form-name form)
	    (form-action form))))

(defmethod initialize-instance :after ((form form) &rest initargs)
  (loop for field in (form-fields form)
     do
       (setf (field-form (cdr field)) form)))

(defclass form-field ()
  ((name :initarg :name
	 :initform (error "Provide the field name")
	 :accessor field-name
	 :documentation "The field name")
   (label :initarg :label
	  :initform nil
	  :accessor field-label
	  :documentation "The field label")
   (value :initarg :value
	  :accessor field-value
	  :documentation "Field value")
   (empty-value :initarg :empty-value
		:initform nil
		:accessor field-empty-value
		:documentation "Value to use when the field value is nil")
   (formatter :initarg :formatter
	      :initform #'princ-to-string
	      :accessor field-formatter
	      :documentation "The field formatter")
   (constraints :initarg :constraints
	      :initform nil
	      :accessor field-constraints
	      :documentation "The field constraints")
   (required :initarg :required-p
	     :initform t
	     :accessor field-required-p
	     :documentation "Whether the field is required")
   (read-only :initarg :read-only-p
	      :initform nil
	      :accessor field-read-only-p
	      :documentation "Whether the field is read only")
   (disabled :initarg :disabled-p
	     :initform nil
	     :accessor field-disabled-p
	     :documentation "Whether the field is disabled")
   (accessor :initarg :accessor
	     :initform nil
	     :accessor field-accessor
	     :documentation "The field accessor to the underlying model")
   (invalid-message :initarg :invalid-message
		    :initform nil
		    :accessor field-invalid-message
		    :documentation "The message to display if the field is invalid")
   (trim :initarg :trim-p
	 :initform t
	 :accessor field-trim-p
	 :documentation "Trim the input")
   (form :initarg :form
	 :initform nil
	 :accessor field-form
	 :documentation "The form the field belongs to"))
  (:documentation "A form field"))

(defmethod print-object ((field form-field) stream)
  (print-unreadable-object (field stream :type t :identity t)
    (format stream "~A value: ~A"
	    (field-name field)
	    (field-value field))))

(defun add-field (form field)
  (setf (form-fields form)
	(append (form-fields form) field)))

(defun form-field-name (form-field &optional (form *form*))
  (format nil "~A-~A" (form-name form) (field-name form-field)))

(defmethod field-value ((field form-field))
  (if (field-accessor field)
      (funcall (fdefinition (field-accessor field))
	       (form-model (field-form field)))
      (slot-value field 'value)))

(defmethod (setf field-value) (value (field form-field))
  (if (field-accessor field)
      (funcall (fdefinition `(setf ,(field-accessor field)))
	       value
	       (form-model (field-form field)))
      (setf (slot-value field 'value) value)))     

(defgeneric validate-form-field (form-field))

(defmethod validate-form-field ((form-field form-field))
  (let ((errors nil))
    (loop for constraint in (field-constraints form-field)
	 do
	 (multiple-value-bind (valid-p error-msg)
	     (funcall constraint
		      (field-value form-field))
	 (when (not valid-p)
	   (push error-msg errors))))
    errors))

(defun validate-form (&optional (form *form*))
  (setf (form-errors form)
	(loop for field in (form-fields form)
	   appending
	     (let ((errors
		    (validate-form-field (cdr field))))
	       (when errors
		 (cons field errors))))))
  
(defun render-form (&optional (form *form*) &rest args)
  (apply #'renderer-render-form *form-renderer* form args))

(defun render-field (field &optional (form *form*) &rest args)
  (apply #'renderer-render-field *form-renderer* field form args))

(defun render-field-label (field &optional (form *form*) &rest args)
  (apply #'renderer-render-field-label *form-renderer* field form args))

(defun render-field-errors (field &optional (form *form*) &rest args)
  (apply #'renderer-render-field-errors *form-renderer* field form args))

(defun render-field-widget (field &optional (form *form*) &rest args)
  (apply #'renderer-render-field-widget *form-renderer* field form args))

(defgeneric renderer-render-form (renderer form &rest args))
(defgeneric renderer-render-field (renderer field form &rest args))
(defgeneric renderer-render-field-label (renderer field form &rest args))
(defgeneric renderer-render-field-errors (renderer field form &rest args))
(defgeneric renderer-render-field-widget (renderer field form &rest argss))

(defun handle-request (&optional (form *form*))
  (loop for field in (form-fields form)
       do (field-read-from-request (cdr field) form)))

(defgeneric field-read-from-request (field form))

(defgeneric make-form-field (field-type &rest args))

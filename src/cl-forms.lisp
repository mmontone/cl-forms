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
  (let ((form-builder (get name :form)))
    (when (not form-builder)
      (error "Form not found: ~A" name))
    (apply form-builder args)))

(defmacro with-form-fields (fields form &body body)
  `(let ,(loop for field in fields
	      collect `(,field (get-field ,form ',field)))
     ,@body))

(defmacro with-form-field-values (fields form &body body)
  `(let ,(loop for field in fields
	      collect `(,field (field-value (get-field ,form ',field))))
     ,@body))

(defun get-field (form field-name &optional (error-p t))
  (or 
   (cdr (assoc field-name (form-fields form)))
   (when error-p
     (error "Field ~A not found in ~A" field-name form))))

(defclass form ()
  ((id :initarg :id
       :accessor form-id
       :initform (string (gensym))
       :documentation "The form id")
   (name :initarg :name
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
	   :documentation "Form errors after validation")
   (client-validation :initarg :client-validation
		      :initform t
		      :accessor client-validation
		      :documentation "When T, form client validation is enabled"))	    
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

(defmethod make-csrf-token ((form form))
  (ironclad:byte-array-to-hex-string
   (ironclad:ascii-string-to-byte-array
    (princ-to-string (uuid:make-v4-uuid)))))

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
	  :initform nil
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
   (validation-triggers :initarg :validation-triggers
			:initform nil
			:accessor field-validation-triggers
			:documentation "Client side validation triggers. A list of :change, :focus, :focusout, :focusin, etc")
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

(defmethod validate-form-field :around ((form-field form-field))
  (cond
    ((and (field-required-p form-field)
	  (null (field-value form-field)))
     (values nil
	     (list (format nil "~A is required" (field-name form-field)))))
    ((and (not (field-required-p form-field))
	  (null (field-value form-field)))
     (values t nil))
    (t (call-next-method))))
    
(defmethod validate-form-field ((form-field form-field))
  (let ((errors nil))
    (loop for constraint in (field-constraints form-field)
       do
	 (multiple-value-bind (valid-p error-msg)
	     (funcall constraint
		      (field-value form-field))
	   (when (not valid-p)
	     (push error-msg errors))))
    (values (not errors)
	    errors)))

(defmethod form-session-csrf-entry ((form form))
  (alexandria:make-keyword (format nil "~A-CSRF-TOKEN" (form-name form))))

(defmethod get-form-session-csrf-token ((form form))
  (hunchentoot:session-value (form-session-csrf-entry form)))

(defmethod set-form-session-csrf-token ((form form))
  (hunchentoot:start-session)
  (setf (hunchentoot:session-value (form-session-csrf-entry form))
	(make-csrf-token form)))

(defun validate-form (&optional (form *form*))
  (setf (form-errors form)
	(loop for field in (form-fields form)
	   appending
	     (multiple-value-bind (valid-p errors)
		    (validate-form-field (cdr field))
	       (when (not valid-p)
		 (list (cons (cdr field) errors))))))
  (values (null (form-errors form))
	  (form-errors form)))

(defun render-form (&optional (form *form*) &rest args)
  (apply #'renderer-render-form *form-renderer* *form-theme* form args))

(defun render-form-start (&optional (form *form*) &rest args)
  (apply #'renderer-render-form-start *form-renderer* *form-theme* form args))

(defun render-form-end (&optional (form *form*))
  (renderer-render-form-end *form-renderer* *form-theme* form))

(defun render-form-errors (&optional (form *form*) &rest args)
  (apply #'renderer-render-form-errors *form-renderer* *form-theme* form args))

(defun render-field (field &optional (form *form*) &rest args)
  (let ((field (if (symbolp field)
		   (get-field form field)
		   field)))
    (apply #'renderer-render-field *form-renderer* *form-theme* field form args)))

(defun render-field-label (field &optional (form *form*) &rest args)
  (let ((field (if (symbolp field)
		   (get-field form field)
		   field)))
    (apply #'renderer-render-field-label *form-renderer* *form-theme* field form args)))

(defun render-field-errors (field &optional (form *form*) &rest args)
  (let ((field (if (symbolp field)
		   (get-field form field)
		   field)))
    (apply #'renderer-render-field-errors *form-renderer* *form-theme* field form args)))

(defun render-field-widget (field &optional (form *form*) &rest args)
  (let ((field (if (symbolp field)
		   (get-field form field)
		   field)))
    (apply #'renderer-render-field-widget *form-renderer* *form-theme* field form args)))

(defgeneric renderer-render-form (renderer theme form &rest args))
(defgeneric renderer-render-form-start (renderer theme form &rest args))
(defgeneric renderer-render-form-end (renderer theme form))
(defgeneric renderer-render-field (renderer theme field form &rest args))
(defgeneric renderer-render-field-label (renderer theme field form &rest args))
(defgeneric renderer-render-field-errors (renderer theme field form &rest args))
(defgeneric renderer-render-field-widget (renderer theme field form &rest argss))

(defun handle-request (&optional (form *form*))
  (when (form-csrf-protection-p form)
    ;; Check the csrf token
    (let ((session-token (get-form-session-csrf-token form)))
      (when (or (not session-token)
		(not (equalp session-token
			     (hunchentoot:post-parameter (form-csrf-field-name form)))))
	(error "Invalid CSRF token"))))
  (loop for field in (form-fields form)
       do (field-read-from-request (cdr field) form
				   (hunchentoot:post-parameters*))))

(defgeneric field-read-from-request (field form parameters))

(defgeneric make-form-field (field-type &rest args))

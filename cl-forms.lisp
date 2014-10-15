;;;; cl-forms.lisp

(in-package #:cl-forms)

(defvar *form-renderer* nil)
(defvar *default-form-renderer* :who)
(defvar *form* nil "The current form")

(defun call-with-form-renderer (renderer function)
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
		    :documentation "csrf field name"))	    
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
   (validator :initarg :validator
	      :initform nil
	      :accessor field-validator
	      :documentation "The field validator")
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

(defclass string-form-field (form-field)
  ()
  (:default-initargs :validator (clavier:is-a-string))
  (:documentation "A string input field"))

(defclass text-form-field (string-form-field)
  ()
  (:default-initargs :validator (clavier:is-a-string))
  (:documentation "A text field. Renders as a text area"))

(defclass boolean-form-field (form-field)
  ()
  (:default-initargs :validator (clavier:is-a-boolean))
  (:documentation "A boolean input"))

(defclass submit-form-field (form-field)
  ()
  (:documentation "A submit input button"))

(defclass choice-form-field (form-field)
  ((choices :initarg :choices
	    :initform nil
	    :accessor field-choices
	    :documentation "An alist with the choices")
   (preferred-choices :initarg :preferred-choices
		      :initform nil
		      :accessor field-preferred-choices
		      :documentation "If this option is specified, then a sub-set of all of the options will be moved to the top of the select menu.")
   (expanded :initarg :expanded
	     :initform nil
	     :accessor field-expanded
	     :documentation "If set to true, radio buttons or checkboxes will be rendered (depending on the multiple value). If false, a select element will be rendered.")
   (multiple :initarg :multiple
	     :initform nil
	     :accessor field-multiple
	     :documentation "If true, the user will be able to select multiple options (as opposed to choosing just one option). Depending on the value of the expanded option, this will render either a select tag or checkboxes if true and a select tag or radio buttons if false.")
   (key-reader :initarg :key-reader
	       :initform #'identity
	       :accessor field-key-reader
	       :documentation "Function to read the option key from the request")) 
  (:documentation "A multi-purpose field used to allow the user to \"choose\" one or more options. It can be rendered as a select tag, radio buttons, or checkboxes."))

(defmethod initialize-instance :after ((field choice-form-field) &rest initargs)
  ;; Initialize the key reader and writer
  (let ((choices (field-choices-alist field)))
    (let ((choice-key (first (first choices))))
      (cond
	((stringp choice-key)
	 (setf (field-key-reader field) #'identity))
	((integerp choice-key)
	 (setf (field-key-reader field) #'parse-integer))
	((keywordp choice-key)
	 (setf (field-key-reader field) #'alexandria:make-keyword))
	(t (error "Invalid key ~A" choice-key))))))	

(defun form-field-name (form-field &optional (form *form*))
  (format nil "~A-~A" (form-name form) (field-name form-field)))

(defgeneric validate-form-field (form-field))

(defmethod validate-form-field ((form-field form-field))
  (when (field-validator form-field)
    (funcall (field-validator form-field)
	     (field-value form-field))))

(defmethod validate-form-field ((form-field string-form-field))
  (and
   (funcall (clavier:is-a-string "~A should be a string" (field-name form-field))
	    (field-value form-field))
   (call-next-method)))

(defun validate-form (&optional (form *form*))
  (loop for field in (form-fields form)
       do (validate-form-field (cdr field))))

(defclass form-view ()
  ((class :initarg :class
	  :initform nil
	  :accessor form-view-class
	  :documentation "The form class attribute")
   (style :initarg :style
	  :initform nil
	  :accessor form-view-style
	  :documentation "The style attribute")
   (form :initarg :form
	 :initform (error "Provide the form")
	 :accessor form-view-form
	 :documentation "The underlying form")
   (form-fields-views :accessor form-fields-views
		      :documentation "The views for each of the fields")))
  
(defun render-form (&optional (form *form*))
  (renderer-render-form *form-renderer* form))

(defun render-field (field &optional (form *form*))
  (renderer-render-field *form-renderer* field form))

(defun render-field-label (field &optional (form *form*))
  (renderer-render-field-label *form-renderer* field form))

(defun render-field-errors (field &optional (form *form*))
  (renderer-render-field-errors *form-renderer* field form))

(defun render-field-widget (field &optional (form *form*))
  (renderer-render-field-widget *form-renderer* field form))

(defgeneric renderer-render-form (renderer form))
(defgeneric renderer-render-field (renderer field form))
(defgeneric renderer-render-field-label (renderer field form))
(defgeneric renderer-render-field-errors (renderer field form))
(defgeneric renderer-render-field-widget (renderer field form))

(defun handle-request (&optional (form *form*))
  (loop for field in (form-fields form)
       do (field-read-from-request (cdr field) form)))

(defgeneric field-read-from-request (field form))

(defmethod field-read-from-request ((field string-form-field) form)
  (setf (field-value field)
	(hunchentoot:post-parameter (form-field-name field form))))

(defmethod field-read-from-request ((field boolean-form-field) form)
  (setf (field-value field)
	(equalp (hunchentoot:post-parameter (form-field-name field form)) "on")))

(defmethod field-read-from-request ((field submit-form-field) form)
  )

(defgeneric make-form-field (field-type &rest args))

(defmethod make-form-field ((field-type (eql :string)) &rest args)
  (apply #'make-instance 'string-form-field args))

(defmethod make-form-field ((field-type (eql :boolean)) &rest args)
  (apply #'make-instance 'boolean-form-field args))

(defmethod make-form-field ((field-type (eql :submit)) &rest args)
  (apply #'make-instance 'submit-form-field args))

(defmethod make-form-field ((field-type (eql :choice)) &rest args)
  (apply #'make-instance 'choice-form-field args))

(defun alistp (alist)
  (and (listp alist)           ; a list
       (every #'consp alist)))

(defmethod field-choices-alist ((field choice-form-field))
  (let ((choices (field-choices field)))
    (if (alistp choices)
	choices
	(mapcar (lambda (choice)
		  (cons (sxhash choice)
			choice))
		choices))))

(defmethod field-key-and-value ((field choice-form-field))
  (when (field-value field)
    (if (alistp (field-choices field))
	(find (field-value field)
	      (field-choices field)
	      :key #'cdr
	      :test #'equalp)
	(cons (sxhash (field-value field))
	      (field-value field)))))

(defmethod field-read-from-request ((field choice-form-field) form)
  (setf (field-value field)
	(cdr (assoc (funcall (field-key-reader field)
			     (hunchentoot:post-parameter (form-field-name field form)))
		    (field-choices-alist field)))))

(in-package #:cl-forms)

(defvar *form-renderer* nil)
(defvar *default-form-renderer* :who)
(defvar *form* nil "The current form")
(defvar *base64-encode* nil "If T, encode form parameters in base64")
(defvar *field-required-message* nil)

(defun check-duplicate-fields (fields)
  (when (not (equal
              (length (remove-duplicates fields
                                         :key (alexandria:compose 'string 'car)
                                         :test 'string=))
              (length fields)))
    (error "Form contains duplicate fields")))

(defmethod call-with-form-renderer (renderer function)
  (let ((*form-renderer* renderer))
    (funcall function)))

(defmacro with-form-renderer (renderer &body body)
  `(call-with-form-renderer ,renderer (lambda () ,@body)))

(defun call-with-form-theme (form-theme function)
  (let ((theme (if (symbolp form-theme)
                   (make-instance form-theme)
                   form-theme)))
    (let ((*form-theme* theme))
      (funcall function))))

(defmacro with-form-theme (form-theme &body body)
  `(call-with-form-theme ,form-theme (lambda () ,@body)))

(defun call-with-form (form function)
  (let ((*form* form))
    (funcall function)))

(defmacro with-form (form &body body)
  `(call-with-form ,form (lambda () ,@body)))

(defmacro defform (form-name args fields)
  "Define a form at top-level"
  (check-duplicate-fields fields)
  (alexandria:with-gensyms (fargs)
    `(setf (get ',form-name :form)
           (lambda (&rest ,fargs)
             (apply #'make-instance
                    'form
                    :name ',form-name
                    :fields (list ,@(loop for field in fields
                                          collect
                                          (destructuring-bind (field-name field-type &rest field-args) field
                                            `(cons ',field-name (make-form-field ,field-type :name ',(intern (string field-name)) ,@field-args)))))
                    (append ,fargs
                            (list ,@args)))))))

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
          (cons field-name (apply #'make-form-field field-type
                                  :name field-name field-args)))))

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

(defun get-field-value (form field-name &optional (error-p t))
  (declare (ignorable error-p))
  (field-value (get-field form field-name)))

(defun set-field-value (form field-name value)
  (setf (field-value (get-field form field-name)) value))

(defclass form ()
  ((id :initarg :id
       :accessor form-id
       :initform (string (gensym))
       :type string
       :documentation "The form id")
   (name :initarg :name
         :accessor form-name
         :initform (error "Provide a name for the form")
         :type symbol
         :documentation "The form name")
   (action :initarg :action
           :initform nil
           :type (or null string)
           :accessor form-action
           :documentation "The form action")
   (method :initarg :method
           :initform :post
           :type (member :get :post)
           :accessor form-method
           :documentation "The form method")
   (enctype :initarg :enctype
            :initform nil
            :type (or null string)
            :accessor form-enctype
            :documentation "Form encoding type. i.e. Use multipart/form-data for file uploads")
   (fields :initarg :fields
           :initform nil
           :accessor form-fields
           :documentation "Form fields")
   (model :initarg :model
          :initform nil
          :accessor form-model
          :documentation "The form model object")
   (csrf-protection :initarg :csrf-protection
                    :initform nil
                    :type boolean
                    :accessor form-csrf-protection-p
                    :documentation "T when csrf protection is enabled")
   (csrf-field-name :initarg :csrf-field-name
                    :initform "_token"
                    :type (or null string)
                    :accessor form-csrf-field-name
                    :documentation "csrf field name")
   (errors :initform nil
           :accessor form-errors
           :type list
           :documentation "Form errors after validation")
   (display-errors :initarg :display-errors
                   :initform (list :list :inline)
                   :type list
                   :accessor display-errors
                   :documentation "A list containing the places where to display errors. Valid options are :list and :inline")
   (client-validation :initarg :client-validation
                      :initform t
                      :type boolean
                      :accessor client-validation
                      :documentation "When T, form client validation is enabled"))
  (:documentation "A form"))

(defmethod print-object ((form form) stream)
  (print-unreadable-object (form stream :type t :identity t)
    (format stream "~A~@{ ~A~}"
            (form-name form)
            (form-action form))))

(defmethod initialize-instance :after ((form form) &rest initargs)
  (declare (ignorable initargs))
  (loop for field in (form-fields form)
        do
           (setf (field-form (cdr field)) form)))

(defun post-parameters (&optional (request hunchentoot:*request*))
  (let ((post-parameters (hunchentoot:post-parameters request)))
    (if *base64-encode*
        (mapcar (lambda (param)
                  (cons (base64:base64-string-to-string (car param) :uri t)
                        (cdr param)))
                post-parameters)
        post-parameters)))

(defmethod make-csrf-token ((form form))
  (ironclad:byte-array-to-hex-string
   (ironclad:ascii-string-to-byte-array
    (princ-to-string (uuid:make-v4-uuid)))))

(defclass form-field ()
  ((name :initarg :name
         :initform (error "Provide the field name")
         :accessor field-name
         :type symbol
         :documentation "The field name")
   (label :initarg :label
          :initform nil
          :accessor field-label
          :type (or null string)
          :documentation "The field label")
   (value :initarg :value
          :initform nil
          :documentation "Field value")
   (default-value :initarg :default-value
                  :initform nil
                  :accessor field-default-value
                  :documentation "Value to use when the field value is nil")
   (placeholder :initarg :placeholder
                :accessor field-placeholder
                :type (or null string)
                :initform nil
                :documentation "Field placeholder (text that appears when the field is empty)")
   (help-text :initarg :help-text
              :initform nil
              :type (or null string)
              :accessor field-help-text
              :documentation "Field help text")
   (parser :initarg :parser
           :initform nil
           :accessor field-parser
           :type (or null symbol function)
           :documentation "Custom field value parser")
   (formatter :initarg :formatter
              :initform nil
              :type (or null symbol function)
              :accessor field-formatter
              :documentation "The field formatter. The function takes two arguments, a VALUE and STREAM to format it into.")
   (constraints :initarg :constraints
                :initform nil
                :accessor field-constraints
                :documentation "The field constraints")
   (required :initarg :required-p
             :initform t
             :type boolean
             :accessor field-required-p
             :documentation "Whether the field is required")
   (required-message :initarg :required-message
                     :initform *field-required-message*
                     :accessor field-required-message
                     :type (or null string)
                     :documentation "Message to display when field is required")
   (invalid-message :initarg :invalid-message
                    :initform nil
                    :accessor field-invalid-message
                    :type (or null string function)
                    :documentation "Message to display when field is invalid")
   (read-only :initarg :read-only-p
              :initform nil
              :type boolean
              :accessor field-read-only-p
              :documentation "Whether the field is read only")
   (disabled :initarg :disabled-p
             :initform nil
             :type boolean
             :accessor field-disabled-p
             :documentation "Whether the field is disabled")
   (accessor :initarg :accessor
             :initform nil
             :type (or null symbol)
             :accessor field-accessor
             :documentation "The field accessor to the underlying model")
   (reader :initarg :reader
           :initform nil
           :type (or null symbol function)
           :documentation "The function to use to read from the underlying model")
   (writer :initarg :writer
           :initform nil
           :type (or null symbol function)
           :documentation "The function to use to write to underlying model")
   (trim :initarg :trim-p
         :initform t
         :type boolean
         :accessor field-trim-p
         :documentation "Trim the input")
   (validation-triggers :initarg :validation-triggers
                        :initform nil
                        :accessor field-validation-triggers
                        :documentation "Client side validation triggers. A list of :change, :focus, :focusout, :focusin, etc")
   (form :initarg :form
         :initform nil
         :type (or null form)
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
        (append (form-fields form)
                (list (cons (field-name field) field)))))

(defun remove-field (form field)
  (setf (form-fields form)
        (remove (if (not (symbolp field))
                    (field-name field)
                    field)
                (form-fields form)
                :key 'car)))

(defmethod field-render-label-p ((field form-field))
  t)

(defvar *field-path* nil)

(defgeneric field-add-to-path (form-field form &optional path)
  (:method (form-field form &optional (path *field-path*))
    (if (null path)
        (cons (list (form-name form) "." (field-name form-field))
              path)
        (cons (field-name form-field) path))))

(defun field-request-name (form-field form)
  (declare (ignorable form-field form))
  (fmt:fmt nil
           (:join "" (alexandria:flatten (reverse *field-path*)))))

(defun render-field-request-name (form-field form)
  (let ((request-name (field-request-name form-field form)))
    (if *base64-encode*
        (base64:string-to-base64-string request-name :uri t)
        request-name)))

(defmethod field-reader ((field form-field))
  (or (slot-value field 'reader)
      (field-accessor field)))

(defmethod field-writer ((field form-field))
  (or (slot-value field 'writer)
      (and (field-accessor field)
           (fdefinition `(setf ,(field-accessor field)))))) 

(defmethod field-value ((field form-field))
  (if (and (field-reader field) (form-model (field-form field)))
      (funcall (field-reader field)
               (form-model (field-form field)))
      (slot-value field 'value)))

(defmethod field-value :around ((field form-field))
  (or (call-next-method)
      (field-default-value field)))

(defmethod (setf field-value) (value (field form-field))
  (if (and (field-writer field) (form-model (field-form field)))
      (funcall (field-writer field)
               value
               (form-model (field-form field)))
      (setf (slot-value field 'value) value)))

(defun fill-form-from-model (form model)
  "Fill a FORM from a MODEL"
  (loop for field in (mapcar 'cdr (forms::form-fields form))
        when (forms:field-reader field)
          do (setf (forms:field-value field)
                   (funcall (forms:field-reader field) model)))
  form)

(defun fill-model-from-form (form model)
  "Set a MODEL's values from FORM field values"
  (loop for field in (mapcar 'cdr (forms::form-fields form))
        when (forms:field-writer field)
          do (funcall (forms:field-writer field)
                      (forms:field-value field)
                      model)))

(defclass field-validator (clavier::validator)
  ((field :initarg :field
          :initform (error "Provide the field")
          :accessor validator-field
          :documentation "The validator field"))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "Generic field validator. Needs a field to be initialized"))

(defun empty-value-p (val)
  (or (null val)
      (and (stringp val)
           (zerop (length val)))))

(defgeneric validate-form-field (form-field))

(defmethod validate-form-field :around ((form-field form-field))
  (cond
    ((and (field-required-p form-field)
          (empty-value-p (field-value form-field)))
     (values nil
             (list (format nil (or (field-required-message form-field)
                                   "~A is required")
                           (or (field-label form-field)
                               (field-name form-field))))))
    ((and (not (field-required-p form-field))
          (empty-value-p (field-value form-field)))
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

(defmethod field-valid-p ((form-field form-field) &optional (form *form*))
  "Determines if a field is valid. This method assumes the form has already
been validated via validate-form."
  (not (find form-field (form-errors form) :key 'car)))

(defgeneric format-field-value (form-field field-value &optional stream))

(defmethod format-field-value :around ((form-field form-field) field-value &optional (stream *standard-output*))
  (if (field-formatter form-field)
      (funcall (field-formatter form-field)
               field-value stream)
      (call-next-method)))

(defmethod format-field-value ((form-field form-field) field-value &optional (stream *standard-output*))
  (format stream "~A" field-value))

(defun format-field-value-to-string (form-field &optional (field-value (field-value form-field)))
  (with-output-to-string (s)
    (format-field-value form-field field-value s)))

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

(defun form-valid-p (form)
  (null (form-errors form)))

(defun add-form-error (field error-msg &optional (form *form*))
  "Add an error on FIELD"
  (let ((field (if (symbolp field)
                   (forms::get-field form field)
                   field)))
    (if (assoc field (form-errors form))
        (push error-msg (cdr (assoc field (form-errors form))))
        (push (cons field (list error-msg))
              (form-errors form)))))

(defun render-form (&optional (form *form*) &rest args)
  (apply #'renderer-render-form *form-renderer* *form-theme* form args))

(defun render-form-start (&optional (form *form*) &rest args)
  (apply #'renderer-render-form-start *form-renderer* *form-theme* form args))

(defun render-form-end (&optional (form *form*))
  (renderer-render-form-end *form-renderer* *form-theme* form))

(defun render-form-errors (&optional (form *form*) &rest args)
  (when (member :list (display-errors form))
    (apply #'renderer-render-form-errors *form-renderer* *form-theme* form args)))

(defun render-field (field &optional (form *form*) &rest args)
  (let ((field (if (symbolp field)
                   (get-field form field)
                   field)))
    (apply #'renderer-render-field *form-renderer* *form-theme* field form args)))

(defun render-field-label (field &optional (form *form*) &rest args)
  (let ((field (if (symbolp field)
                   (get-field form field)
                   field)))
    (unless (typep field 'submit-form-field)
      (apply #'renderer-render-field-label *form-renderer* *form-theme* field form args))))

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
(defgeneric renderer-render-form-errors (renderer theme form &rest args))
(defgeneric renderer-render-field (renderer theme field form &rest args))
(defgeneric renderer-render-field-label (renderer theme field form &rest args))
(defgeneric renderer-render-field-errors (renderer theme field form &rest args))
(defgeneric renderer-render-field-widget (renderer theme field form &rest argss))

(defmethod renderer-render-field-errors :around (renderer theme field form &rest args)
  (declare (ignorable args))
  ;; Render field errors inline only when :inline is specified on form
  (when (member :inline (forms::display-errors form))
    (call-next-method)))

(defmethod renderer-render-form-errors :around  (renderer theme form &rest args)
  (declare (ignorable args))
  ;; Render form errors only when :list is specified on form
  (when (member :list (forms::display-errors form))
    (call-next-method)))

(defmethod renderer-render-field-widget :around (renderer theme field form &rest args)
  (declare (ignorable args))
  (let ((*field-path* (field-add-to-path field form *field-path*)))
    (call-next-method)))

(defun handle-request (&optional (form *form*))
  (when (form-csrf-protection-p form)
    ;; Check the csrf token
    (let ((session-token (get-form-session-csrf-token form)))
      (when (or (not session-token)
                (not (equalp session-token
                             (hunchentoot:post-parameter (form-csrf-field-name form)))))
        ;; The form is not valid. Throw an error, but reset its CSRF token for next time
        (set-form-session-csrf-token form)
        (error "Invalid CSRF token"))))
  (let ((post-parameters (post-parameters)))
    (loop for field in (form-fields form)
          do (field-read-from-request (cdr field) form
                                      post-parameters))))

(defgeneric field-read-from-request (field form parameters))

(defmethod field-read-from-request :around (field form parameters)
  (let ((*field-path* (field-add-to-path field form *field-path*)))
    (call-next-method)))

(defmethod field-read-from-request :after (field form parameters)
  "Use the parser function after reading the value from the request"
  (when (field-parser field)
    (setf (field-value field)
          (funcall (field-parser field)
                   (field-value field)))))

(defgeneric make-form-field (field-type &rest args))

;; Form templates

(defun collect-replace-fields (form)
  (let (fields)
    (let ((new-form (%collect-replace-fields
                     form
                     (lambda (field) (push field fields)))))
      (values new-form fields))))

(defun %collect-replace-fields (form collect-field)
  (if (atom form)
      form
      (if (eql (first form) 'forms:form-field)
          (progn
            (funcall collect-field (cdr form))
            `(forms:render-field ',(second form)))
          (loop for part in form
                collect
                (%collect-replace-fields part collect-field)))))

(defmacro with-form-template ((&optional form-var) form-name args &body body)
  (multiple-value-bind (new-body fields) (collect-replace-fields body)
    (let ((form-bind (or form-var (gensym "FORM"))))
      `(progn
         (defform ,form-name ,args
           ,fields)
         (let ((,form-bind (or ,form-var (get-form ',form-name))))
           (with-form ,form-bind
             (render-form-start)
             ,@new-body
             (render-form-end)))))))

(defun make-formatter (symbol)
  "Create a field formatter. SYMBOL is the function to call."
  (lambda (x stream)
    (format stream "~A" (funcall symbol x))))

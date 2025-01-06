(in-package #:cl-forms)

(defvar *form-renderer* nil "The current form renderer.
Bind using WITH-FORM-RENDERER.")
(defvar *default-form-renderer* :who
  "The default form renderer.
The default renderer is CL-WHO.")
(defvar *form* nil "The current form.")
(defvar *base64-encode* nil "Whether to encode form parameters in base64 or not.")
(defvar *field-required-message* nil
  "The default message to displayed when a field is required.")

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
  "Bind *FORM-RENDERER* to RENDERER and evaluate BODY in that context."
  `(call-with-form-renderer ,renderer (lambda () ,@body)))

(defun call-with-form-theme (form-theme function)
  (let ((theme (if (symbolp form-theme)
                   (make-instance form-theme)
                   form-theme)))
    (let ((*form-theme* theme))
      (funcall function))))

(defmacro with-form-theme (form-theme &body body)
  "Bind *FORM-THEME* to FORM-THEME and evaluate BODY in that context."
  `(call-with-form-theme ,form-theme (lambda () ,@body)))

(defun call-with-form (form function)
  (let ((*form* form))
    (funcall function)))

(defmacro with-form (form &body body)
  "Bind *FORM* to FORM and evaluate BODY in that context."
  `(call-with-form ,form (lambda () ,@body)))

(defmacro defform (form-name args fields)
  "Define a form at top-level.

ARGS are the arguments passed to FORM class via MAKE-INSTANCE.
FIELDS are the form field specs.

@lisp
(forms:defform client-validated-form (:action \"/client-validation-post\"
                                              :client-validation t)
  ((name :string :value \"\" :constraints (list (clavier:is-a-string)
                                              (clavier:not-blank)
                                              (clavier:len :max 5))
         :validation-triggers '(:focusin))
   (single :boolean :value t)
   (sex :choice :choices (list \"Male\" \"Female\") :value \"Male\")
   (age :integer :constraints (list (clavier:is-an-integer)
                                    (clavier:greater-than -1)
                                    (clavier:less-than 200)))
   (email :email)
   (submit :submit :label \"Create\")))
@end lisp"
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
  "Registers a function with arguments ARGS and body BODY as a form builder.

BODY is expected to instantiate a FORM object using ARGS in some way.

FORM-NAME is the symbol under which the FORM is registered.

Use FIND-FORM with FORM-NAME and expected arguments to obtain the registered form."
  (alexandria:with-unique-names (form)
    `(setf (get ',form-name :form)
           (lambda ,args
             (let ((,form (progn ,@body)))
               (check-type ,form forms:form)
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

(defun find-form (name &rest args)
  "Get the form named NAME.

ARGS is the list of arguments to pass to a possible form builder function.

See: DEFFORM-BUILDER macro."
  (let ((form-builder (get name :form)))
    (when (not form-builder)
      (error "Form not found: ~A" name))
    (apply form-builder args)))

(defun get-form (&rest args)
  (warn "FORMS:GET-FORM is deprecated. Use FORMS:FIND-FORM instead.")
  (apply #'find-form args))

(defmacro with-form-fields (fields form &body body)
  "Bind FIELDS to the form fields in FORM.

Example:

@lisp
(with-form-fields (name) form
   (print (field-value name)))
@end lisp

Also see: WITH-FORM-FIELD-VALUES "
  `(let ,(loop for field in fields
               collect `(,field (get-field ,form ',field)))
     ,@body))

(defmacro with-form-field-values (fields form &body body)
  "Bind the value of FIELDS in FORM.

Example:

@lisp
(with-form-field-values (name) form
   (print name))
@end lisp"
  `(let ,(loop for field in fields
               collect `(,field (field-value (get-field ,form ',field))))
     ,@body))

(defun get-field (form field-name &optional (error-p t))
  (or
   (cdr (assoc field-name (form-fields form)))
   (when error-p
     (error "Field ~S not found in ~A" field-name form))))

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
           :documentation "Form errors after validation. An association list with elements (<field> . <field errors strings list>).")
   (display-errors :initarg :display-errors
                   :initform (list :list :inline)
                   :type list
                   :accessor display-errors
                   :documentation "A list containing the places where to display errors. Valid options are :list and :inline")
   (client-validation :initarg :client-validation
                      :initform nil
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

(defun post-parameters (&optional (request (backend-request)))
  (let ((post-parameters (request-post-parameters request)))
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
                :documentation "A list of CLAVIER validators.")
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
    (format stream "~S value: ~A"
            (field-name field)
            (field-value field))))

(defmethod field-label ((field form-field))
  (or (slot-value field 'label)
      (str:sentence-case (princ-to-string (field-name field)))))

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
    (cons (string-downcase (string (field-name form-field))) path)))

(defun field-request-name (form-field form)
  (declare (ignorable form form-field))
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
  "Fill a FORM from a MODEL.
Read MODEL using FORM accessors and set the FORM field values."
  (loop for field in (mapcar 'cdr (forms::form-fields form))
        when (forms:field-reader field)
          do (setf (forms:field-value field)
                   (funcall (forms:field-reader field) model)))
  form)

(defun fill-model-from-form (form model)
  "Set a MODEL's values from FORM field values."
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
  (:documentation "Generic field validator. Needs a field to be initialized."))

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

(defun validate-form (&optional (form *form*))
  "Validates a form. Usually called after HANDLE-REQUEST. Returns multiple values; first value is true if the form is valid; second value a list of errors.
The list of errors is an association list with elements (<field> . <field errors strings list>)."
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
  "Top level function to render the web form FORM.
*FORM-RENDERER* and *FORM-THEME* need to be bound.
See: WITH-FORM-RENDERER, WITH-FORM-THEME"
  (apply #'renderer-render-form
	 (or *form-renderer* (error "Form renderer is unbound. See *FORM-RENDERER* and WITH-FORM-RENDERER."))
	 *form-theme* form args))

(defun render-form-start (&optional (form *form*) &rest args)
  "Render only the beggining of the web form FORM.
Use RENDER-FIELD, RENDER-FIELD-LABEL, etc manually, after."
  (apply #'renderer-render-form-start
	 (or *form-renderer*
	     (error "Form renderer is unbound. See *FORM-RENDERER* and WITH-FORM-RENDERER."))
	 *form-theme* form args))

(defun render-form-end (&optional (form *form*))
  "Render the end of the web form FORM."
  (renderer-render-form-end *form-renderer* *form-theme* form))

(defun render-form-errors (&optional (form *form*) &rest args)
  "Render a section for displaying form validation errors."
  (when (member :list (display-errors form))
    (apply #'renderer-render-form-errors *form-renderer* *form-theme* form args)))

(defun render-field (field &optional (form *form*) &rest args)
  "Render form FIELD, both label and widget."
  (let ((field (if (symbolp field)
                   (get-field form field)
                   field)))
    (apply #'renderer-render-field *form-renderer* *form-theme* field form args)))

(defun render-field-label (field &optional (form *form*) &rest args)
  "Render the label of FIELD."
  (let ((field (if (symbolp field)
                   (get-field form field)
                   field)))
    (unless (typep field 'submit-form-field)
      (apply #'renderer-render-field-label *form-renderer* *form-theme* field form args))))

(defun render-field-errors (field &optional (form *form*) &rest args)
  "Render the validation errors associated with FIELD."
  (let ((field (if (symbolp field)
                   (get-field form field)
                   field)))
    (apply #'renderer-render-field-errors *form-renderer* *form-theme* field form args)))

(defun render-field-widget (field &optional (form *form*) &rest args)
  "Render FIELD widget."
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

(defgeneric backend-request ()
  (:documentation "Return current request object for backend"))

(defgeneric backend-handle-request (form request)
  (:documentation "Handle REQUEST for FORM.
Populates FORM from parameters in HTTP request. After this, the form field contains values, but they are not validated. To validate call VALIDATE-FORM after."))

;; Abstract away the HTTP request, so we can make cl-forms testable

(defgeneric request-post-parameters (request))

(defun handle-request (&optional (form *form*) (request (backend-request)))
  "Populates FORM from parameters in HTTP request. After this, the form field contains values, but they are not validated. To validate call VALIDATE-FORM after."
  (backend-handle-request form request))

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

(defmethod make-form-field (field-type &rest args)
  "Provide a good error message when a field cannot be built"
  (if (and (symbolp field-type) (not (keywordp field-type)))
      (apply #'make-instance field-type args)
      ;; else
      (error "Form field of type ~s not defined. Check that ~s is a correct field type." field-type field-type)))

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
  "Define a FORM named FORM-NAME and render it at the same time."
  (multiple-value-bind (new-body fields) (collect-replace-fields body)
    (let ((form-bind (or form-var (gensym "FORM"))))
      `(progn
         (defform ,form-name ,args
           ,fields)
         (let ((,form-bind (or ,form-var (find-form ',form-name))))
           (with-form ,form-bind
             (render-form-start)
             ,@new-body
             (render-form-end)))))))

(defun make-formatter (symbol)
  "Create a field formatter. SYMBOL is the function to call."
  (lambda (x stream)
    (format stream "~A" (funcall symbol x))))

;; Type declarations

(declaim
 (ftype (function (form t) (values t &optional)) fill-form-from-model)
 (ftype (function (form t) (values null &optional)) fill-model-from-form)
 (ftype (function (form) list) form-fields)
 (ftype (function (&optional form &rest t) string) render-form)
 (ftype (function (&optional form &rest t) string) render-form-errors)
 (ftype (function (form-field &optional form &rest t) string) render-field-errors)
 (ftype (function (form-field) *) field-value)
 (ftype (function (form-field) (or null symbol)) field-accessor)
 (ftype (function ((or symbol form-field) &optional form &rest t) string) render-field)
 (ftype (function (form-field) string) field-label)
 (ftype (function (form symbol &optional boolean) (or form-field null)) get-field)
 (ftype (function (form-field) (or null symbol function)) field-parser)
 (ftype (function (form-field &optional t) (values simple-string &optional))
  cl-forms:format-field-value-to-string)
 (ftype (function (form (or symbol form-field) &optional boolean) *) cl-forms:get-field-value)
 (ftype function cl-forms:render-form-start)
 (ftype (function (&optional t) *) cl-forms:render-form-end)
 (ftype (function (t) (values function &optional)) cl-forms:make-formatter)
 (ftype (function (&optional form) (values boolean t &optional))
  cl-forms:validate-form)
 (ftype (function (form-field t &optional t) *) cl-forms:format-field-value)
 (ftype (function (form) boolean) cl-forms:form-valid-p)
 (ftype (function (t) *) cl-forms:field-formatter)
 (ftype (function (form) list) form-errors)
 (ftype (function (symbol &rest t) form) get-form find-form)
 (ftype (function (t &rest t) *) cl-forms:render-field-widget)
 (ftype (function (t) *) cl-forms:field-reader)
 (ftype (function (t) *) cl-forms:field-writer)
 (ftype (function ((or symbol form-field) &optional form) boolean) cl-forms:field-valid-p)
 (ftype (function ((or symbol form-field) &optional form &rest t) *) cl-forms:render-field-label)
 (ftype (function (t t &optional t) *) cl-forms:add-form-error)
 (ftype (function (t t t) *) cl-forms:set-field-value)
 (ftype (function (form (or symbol form-field)) *) remove-field)
 (ftype (function (form form-field) *) cl-forms:add-field))

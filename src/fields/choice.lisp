(in-package :forms)

(defclass choice-form-field (form-field)
  ((choices :initarg :choices
            :initform nil
            :writer (setf field-choices)
            :type (or list symbol function)
            :documentation "An alist with the choices. Or a function with which to obtain the choices.")
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
               :documentation "Function to read the option key from the request")
   (hash-function :initarg :hash-function
                  :initform #'sxhash
                  :accessor field-hash-function
                  :documentation "The function to use for choices key")
   (test :initarg :test
     :initform #'eql
     :accessor field-test
     :documentation "Function to test equality between choices")
   (use-key-as-value :initarg :use-key-as-value
                     :initform nil
                     :accessor use-key-as-value
                     :documentation "When T, use the key/s of the field as value of the field when it is read from request"))
  (:documentation "A multi-purpose field used to allow the user to \"choose\" one or more options. It can be rendered as a select tag, radio buttons, or checkboxes.
NOTE: the defaults of this field type are too complicated for just working with string choices. STRING-CHOICE-FIELD is more convenient for that."))

(defclass string-choice-form-field (choice-form-field)
  ()
  (:default-initargs
   :test 'string=
   :hash-function 'identity
   :use-key-as-value t)
  (:documentation "A choice field for working with string options"))

(defmethod initialize-instance :after ((field choice-form-field) &rest initargs)
  (declare (ignorable initargs))
  ;; Initialize the key reader and writer
  (let ((choices (field-choices-alist field)))
    (when choices
      (let ((choice-key (first (first choices))))
        (cond
          ((stringp choice-key)
           (setf (field-key-reader field) #'identity))
          ((integerp choice-key)
           (setf (field-key-reader field)
                 (lambda (x) (parse-integer x :junk-allowed t))))
          ((keywordp choice-key)
           (setf (field-key-reader field) #'alexandria:make-keyword))
          (t (error "Invalid key: ~A" choice-key))))))
  ;; Add the choices constraint
  (push
   (make-instance 'choice-field-validator
                  :field field)
   (field-constraints field)))

(defmethod field-choices ((field choice-form-field))
  (let ((choices (slot-value field 'choices)))
    (typecase choices
      (list choices)
      (symbol (funcall (fdefinition choices)))
      (function (funcall choices)))))

(defclass choice-field-validator (field-validator)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs :message "The option is not valid"))

(defmethod clavier::%validate ((validator choice-field-validator) object &rest args)
  (declare (ignorable args))
  (let ((valid-choices (field-choices (validator-field validator))))
    (if (forms::field-multiple (validator-field validator))
        (every (lambda (elem)
                 (member elem valid-choices :test
                         (field-test (validator-field validator))))
               object)
        (member object valid-choices :test
                (field-test
                 (validator-field validator))))))

(defmethod make-form-field ((field-type (eql :choice)) &rest args)
  (apply #'make-instance 'choice-form-field args))

(defmethod make-form-field ((field-type (eql :string-choice)) &rest args)
  (apply #'make-instance 'string-choice-form-field args))

(defun alistp (alist)
  (and (listp alist)           ; a list
       (every #'consp alist)))

(defmethod field-choices-alist ((field choice-form-field))
  (let ((choices (field-choices field)))
    (if (alistp choices)
        choices
        (mapcar (lambda (choice)
                  (cons (funcall (field-hash-function field)
                                 choice)
                        choice))
                choices))))

(defmethod field-key-and-value ((field choice-form-field))
  (when (field-value field)
    (if (alistp (field-choices field))
        (find (field-value field)
              (field-choices field)
              :key (if (use-key-as-value field) 'car 'cdr)
              :test #'equalp)
        (cons (funcall (field-hash-function field)
                       (field-value field))
              (field-value field)))))

(defmethod field-keys-and-values ((field choice-form-field))
  (assert (listp (field-value field)))
  (if (alistp (field-value field))
      (field-value field)
      (if (use-key-as-value field)
          (let ((choices-alist (field-choices-alist field)))
            (mapcar (lambda (key)
                      (cons key
                            (cdr (assoc key choices-alist))))
                    (field-value field)))
          (mapcar (lambda (value)
                    (cons (funcall (field-hash-function field)
                                   value)
                          value))
                  (field-value field)))))

(defmethod field-read-from-request ((field choice-form-field) form parameters)
  (cond
    ((and (field-expanded field)
          (field-multiple field))
     ;; Checkboxes rendered
     (let ((selected-keys (mapcar (alexandria:compose (field-key-reader field)
                                                      #'cdr)
                                  (remove-if-not (lambda (param)
                                                   (equalp (first param)
                                                           (string (field-request-name field form))))
                                                 parameters)))
           (choices-alist (field-choices-alist field)))
       (setf (field-value field)
             (if (use-key-as-value field)
                 selected-keys
                 (mapcar (lambda (key)
                           (cdr (assoc key choices-alist)))
                         selected-keys)))))
    ((and (field-expanded field)
          (not (field-multiple field)))
     ;; Radio buttons rendered
     (setf (field-value field)
           (if (use-key-as-value field)
               (funcall (field-key-reader field)
                        (cdr (assoc (field-request-name field form) parameters :test #'string=)))
               (cdr (assoc (funcall (field-key-reader field)
                                    (cdr (assoc (field-request-name field form) parameters :test #'string=)))
                           (field-choices-alist field)
                           :test 'equalp)))))
    ((and (not (field-expanded field))
          (field-multiple field))
     ;; Multiple select box rendered
     (let ((selected-keys (mapcar (alexandria:compose (field-key-reader field)
                                                      #'cdr)
                                  (remove-if-not (lambda (param)
                                                   (equalp (first param)
                                                           (string (field-request-name field form))))
                                                 parameters)))
           (choices-alist (field-choices-alist field)))
       (setf (field-value field)
             (if (use-key-as-value field)
                 selected-keys
                 (mapcar (lambda (key)
                           (cdr (assoc key choices-alist)))
                         selected-keys)))))
    ((and (not (field-expanded field))
          (not (field-multiple field)))
     ;; Single select box
     (setf (field-value field)
           (if (use-key-as-value field)
               (funcall (field-key-reader field)
                        (cdr (assoc (field-request-name field form) parameters :test #'string=)))
               (cdr (assoc (funcall (field-key-reader field)
                                    (cdr (assoc (field-request-name field form) parameters :test #'string=)))
                           (field-choices-alist field)
                           :test 'equalp)))))))

(in-package :forms)

(defclass list-form-field (form-field)
  ((type :initarg :type
         :initform (error "Provide the list type via :type")
         :accessor list-field-type
         :type function
         :documentation "The list elements type.")
   (add-button :initarg :add-button
               :initform t
               :accessor add-button-p
               :type boolean
               :documentation "Whether have a list 'ADD' button or not")
   (remove-button :initarg :remove-button
                  :initform t
                  :accessor remove-button-p
                  :type boolean
                  :documentation "Whether add an item removal button or not"))
  (:documentation "A field that contains a list of elements (either other fields or subforms)"))

(defmethod make-form-field ((field-type (eql :list)) &rest args)
  (let ((list-field-type (if (listp (getf args :type))
                             (lambda ()
                               (apply #'make-form-field
                                      (first (getf args :type))
                                      (append (list :name (getf args :name))
                                              (rest (getf args :type)))))
                             (getf args :type))))
    (apply #'make-instance 'list-form-field :type list-field-type args)))

(defmethod field-read-from-request ((field list-form-field) form parameters)
  (let ((regex
         (ppcre:create-scanner `(:sequence ,(field-request-name field form)
                                           "["
                                           (:register (:greedy-repetition 1 nil :digit-class))
                                           "]"))))
    (let ((request-list-indexes
           (loop for param in parameters
              when (ppcre:scan regex (car param))
              collect (ppcre:register-groups-bind (index)
                          (regex (car param))
                        (parse-integer index)))))
      (let ((items
             (mapcar (lambda (i) 
                       (dflet ((field-request-name (field form)
                                                   (fmt:fmt nil                    
                                                            (call-next-function)                                                              "[" i "]")))
                         (let ((item-field (funcall (list-field-type field))))
                           (field-read-from-request item-field
                                                    form
                                                    parameters)
                           item-field)))
                     request-list-indexes)))
        ;; The value of a list field is a list of fields (the type of its list elements)
        (setf (field-value field) items)))))

(defun list-field-values (list-field)
  "Returns the actual values of a list field"
  (mapcar #'field-value (field-value list-field)))

(in-package :forms)

(defclass file-form-field (form-field)
  ((multiple :initarg :multiple-p
             :accessor multiple-p
             :initform nil
             :documentation "If this fields handles multiple file uploads")
   (path :initarg :path
         :accessor file-path
         :initform nil)
   (file-name :initarg :file-name
              :accessor file-name
              :initform nil)
   (content-type :initarg :content-type
                 :accessor file-content-type
                 :initform nil)
   (upload-handler :initarg :upload-handler
                    :accessor upload-handler
                    :initform nil
                    :documentation "Function that handles the file upload"))
  (:documentation "A file input field"))

(defmethod field-read-from-request ((field file-form-field) form parameters)
  (let ((fvalue
         (cdr (assoc (field-request-name field form) parameters :test #'string=))))
    (if (and fvalue (listp fvalue))
        (destructuring-bind (path file-name content-type) fvalue
          (setf (file-path field) path)
          (setf (file-name field) file-name)
          (setf (file-content-type field) content-type)
          (when (upload-handler field)
            (funcall (upload-handler field) field)))
        ;; else
        (setf (field-value field) fvalue))))

(defmethod make-form-field ((field-type (eql :file)) &rest args)
  (apply #'make-instance 'file-form-field args))

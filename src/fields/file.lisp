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
                   :documentation "Function that handles the file upload")
   (accept :initarg :accept
           :accessor file-accept
           :type (or null string)
           :initform nil
           :documentation "Files accepted. See https://www.w3schools.com/tags/att_input_accept.asp"))
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
        (progn
          (warn "CL-FORMS: Could not read file field file: ~a." field)
          (when (not (equalp (form-enctype form) "multipart/form-data"))
            (warn "CL-FORMS: Encoding type in forms should be set to 'multipart/form-data' for file uploads."))))
    ;; Always set the field value
    (setf (field-value field) fvalue)))

(defmethod make-form-field ((field-type (eql :file)) &rest args)
  (apply #'make-instance 'file-form-field args))

;; Stateful file field

(defclass stateful-file-field (forms::file-form-field)
  ((download-link :initarg :download-link
                  :type (or null string symbol function)
                  :initform nil
                  :documentation "Function to generate the download link to the file"))
  (:documentation "An stateful file field is a file field that mantains its value (the uploaded file data) and renders the the uploaded file download link and a file upload widget"))

(defmethod download-link ((field stateful-file-field))
  (with-slots (download-link) field
    (if (stringp download-link)
        download-link
        (funcall download-link field))))

(defmethod forms::field-read-from-request ((field stateful-file-field) form parameters)
  (let ((fvalue
          (cdr (assoc (forms::field-request-name field form) parameters :test #'string=))))
    (if (and fvalue (listp fvalue))
        ;; A file was uploaded
        (destructuring-bind (path file-name content-type) fvalue
          (setf (forms::file-path field) path)
          (setf (forms::file-name field) file-name)
          (setf (forms::file-content-type field) content-type)
          (when (forms::upload-handler field)
            (funcall (forms::upload-handler field) field)))
        ;; else, recover file info, if it is there
        (let ((file-info
                (cdr (assoc (format nil "~A.info" (forms::field-request-name field form)) parameters :test #'string=))))
          (when file-info
            (let ((finfo (read-from-string (base64:base64-string-to-string file-info))))
              (setf (forms::file-path field) (getf finfo :path)
                    (forms::file-name field) (getf finfo :file-name)
                    (forms::file-content-type field) (getf finfo :content-type))))))))

(defmethod forms::make-form-field ((field-type (eql :stateful-file)) &rest args)
  (apply #'make-instance 'stateful-file-field args))

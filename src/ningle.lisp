(defpackage :cl-forms.ningle
  (:nicknames :forms.ningle)
  (:use :cl :forms)
  (:export #:backend-request
           #:handle-request
           #:request-post-parameters
           #:get-form-session-csrf-token
           #:set-form-session-csrf-token))

(in-package :cl-forms.ningle)

(defmethod backend-request ()
  ningle:*request*)

(defun handle-request (&optional (form *form*) (request ningle:*request*))
  (when (forms::form-csrf-protection-p form)
    ;; Check the csrf token
    (let ((session-token (get-form-session-csrf-token form)))
      (when (or (not session-token)
                (not (equalp session-token (cdr (assoc (forms::form-csrf-field-name form) (request-post-parameters request) :test #'string=)))))
        ;; The form is not valid. Throw an error, but reset its CSRF token for next time
        (forms::set-form-session-csrf-token form)
        (error "Invalid CSRF token"))))

  (let ((post-parameters (lack/request:request-body-parameters request)))
    (loop for field in (form-fields form)
          do (forms::field-read-from-request (cdr field) form
                                             post-parameters))))

(defmethod request-post-parameters ((request lack/request:request))
  (lack/request:request-body-parameters request))

(defun make-csrf-token ()
  (ironclad:byte-array-to-hex-string
   (ironclad:ascii-string-to-byte-array
    (princ-to-string (uuid:make-v4-uuid)))))

(defmethod get-form-session-csrf-token ((form form))
  (gethash (forms::form-session-csrf-entry form) ningle:*session*))

(defmethod forms::set-form-session-csrf-token ((form form))
  (setf (gethash (forms::form-session-csrf-entry form) ningle:*session*) (make-csrf-token)))

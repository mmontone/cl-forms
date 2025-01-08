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
                (not (equalp session-token (cdr (assoc (form-csrf-field-name form) (request-post-parameters request) :test #'string=)))))
        ;; The form is not valid. Throw an error, but reset its CSRF token for next time
        (set-form-session-csrf-token form)
        (error "Invalid CSRF token"))))

  (let ((post-parameters (lack/request:request-body-parameters request)))
    (loop for field in (form-fields form)
          do (forms::field-read-from-request (cdr field) form
                                      post-parameters))))

(defmethod request-post-parameters ((request lack/request:request))
  (lack/request:request-body-parameters request))

(defmethod get-form-session-csrf-token ((form form))
  (lack/middleware/csrf:csrf-token ningle:*session*))

(defmethod set-form-session-csrf-token ((form form))
  (remhash lack/middleware/csrf::*csrf-session-key* ningle:*session*)
  (lack/middleware/csrf:csrf-token ningle:*session*))

(in-package :cl-forms)

(defmethod backend-request ()
  hunchentoot:*request*)

(defmethod backend-handle-request (form request)
  (when (form-csrf-protection-p form)
    ;; Check the csrf token
    (let ((session-token (get-form-session-csrf-token form)))
      (when (or (not session-token)
                (not (equalp session-token
                             (hunchentoot:post-parameter (form-csrf-field-name form) request))))
        ;; The form is not valid. Throw an error, but reset its CSRF token for next time
        (set-form-session-csrf-token form)
        (error "Invalid CSRF token"))))
  (let ((post-parameters (post-parameters request)))
    (loop for field in (form-fields form)
          do (field-read-from-request (cdr field) form
                                      post-parameters))))

(defmethod request-post-parameters ((request hunchentoot:request))
  (hunchentoot:post-parameters request))

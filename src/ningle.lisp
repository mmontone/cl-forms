(defpackage :cl-forms.ningle
  (:nicknames :forms.ningle)
  (:use :cl :forms)
  (:export #:backend-request))

(in-package :cl-forms.ningle)

(defmethod backend-request ()
  nil)

(defun handle-request (&optional (form *form*) (request lack/request:request))
  ;; (when (form-csrf-protection-p form)
  ;;   ;; Check the csrf token
  ;;   (let ((session-token (get-form-session-csrf-token form)))
  ;;     (when (or (not session-token) (not (equalp session-token (cdr (assoc (form-csrf-field-name form) (lack/request:request-body-parameters request) :test #'string=)))))
  ;;       ;; The form is not valid. Throw an error, but reset its CSRF token for next time
  ;;       (set-form-session-csrf-token form)
  ;;       (error "Invalid CSRF token"))))

  ;; (let ((post-parameters (lack/request:request-body-parameters request)))
  ;;   (loop for field in (form-fields form)
  ;;         do (field-read-from-request (cdr field) form
  ;;                                     post-parameters))))
  nil)

(defmethod request-post-parameters ((request lack/request:request))
  ;; (hunchentoot:post-parameters request))
  nil)

(defmethod get-form-session-csrf-token ((form form))
  ;; (hunchentoot:session-value (form-session-csrf-entry form)))
  nil)

(defmethod set-form-session-csrf-token ((form form))
  ;; (hunchentoot:start-session)
  ;; (setf (hunchentoot:session-value (form-session-csrf-entry form))
  ;;       (make-csrf-token form)))
  nil)

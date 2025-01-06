(in-package :cl-forms)

(defvar *backend*) ;; The REQUEST backend implementation

(defgeneric backend-handle-request (form request)
  (:documentation "Handle REQUEST for FORM"))

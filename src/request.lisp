(in-package :cl-forms)

;; Abstract away the HTTP request, so we can make cl-forms testable

(defgeneric request-post-parameters (request))

(defmethod request-post-parameters ((request hunchentoot:request))
  (hunchentoot:post-parameters request))

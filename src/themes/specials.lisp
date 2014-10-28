(in-package :forms)

(defvar *form-theme* (make-instance 'default-form-theme))

(defun call-with-form-theme (form-theme function)
  (let ((*form-theme* form-theme))
    (funcall function)))

(defmacro with-form-theme (form-theme &body body)
  `(call-with-form-theme ,form-theme (lambda () ,@body)))


(defpackage :cl-forms.djula
  (:nicknames :forms.djula)
  (:use :cl :forms :djula))

(in-package :forms.djula)

(interpol:enable-interpol-syntax)

(djula::def-tag-compiler :form (form-name &rest args)
  "Render a form"
  (lambda (stream)
    (let ((form
	   (getf djula::*template-arguments* form-name)))
      (let ((forms::*form-renderer* :who)
	    (forms.who::*html* stream))
	(apply #'forms::render-form form args)))))

(djula::def-tag-compiler :form-start (form-name &rest args)
  "Start form rendering"
  (lambda (stream)
    (let ((form
	   (getf djula::*template-arguments* form-name)))
      (format stream #?'<form action="${(forms::form-action form)}"
	                      method="${(forms::form-method form)}">'))))

(djula::def-tag-compiler :form-end (&optional form-name)
  "Finish form rendering"
  (lambda (stream)
    (format stream "</form>")))

(djula:def-tag-compiler :form-row (form-name field-name &rest args)
  (let ((form
	 (getf djula::*template-arguments* form-name)))
    (let ((field-symbol (intern (string-upcase field-name) *djula-execute-package*)))
      (let ((field (forms::get-field form field-symbol)))
	(lambda (stream)
	  (let ((forms::*form-renderer* :who)
		(forms.who::*html* stream))
	    (apply #'forms::render-field field form args)))))))

(djula:def-tag-compiler :form-field-label (form-name field-name)
  (let ((form
	 (getf djula::*template-arguments* form-name)))
    (let ((field-symbol (intern (string-upcase field-name) *djula-execute-package*)))
      (let ((field (forms::get-field form field-symbol)))
	(lambda (stream)
	  (let ((forms::*form-renderer* :who)
		(forms.who::*html* stream))
	    (forms::render-field-label field form)))))))

(djula:def-tag-compiler :form-field-widget (form-name field-name &rest args)
  (let ((form
	 (getf djula::*template-arguments* form-name)))
    (let ((field-symbol (intern (string-upcase field-name) *djula-execute-package*)))
      (let ((field (forms::get-field form field-symbol)))
	(lambda (stream)
	  (let ((forms::*form-renderer* :who)
		(forms.who::*html* stream))
	    (apply #'forms::render-field-widget field form args)))))))


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
              method="${(forms::form-method form)}"
          ${(if (forms::form-enctype form)
                (format nil " enctype=\"~A\"" (forms::form-enctype form))
                "")}
	      ${(if (getf args :class)
		    (format nil " class=\"~A\"" (getf args :class))
		    "")}>')
      (when (forms::form-csrf-protection-p form)
	(let ((token (forms::set-form-session-csrf-token form)))
	  (format stream #?'<input name="${(forms::form-csrf-field-name form)}"
			type="hidden" value="${token}">'))))))

(djula::def-tag-compiler :form-end (&optional form-name)
  "Finish form rendering"
  (lambda (stream)
    (format stream "</form>")))

(djula::def-tag-compiler :form-errors (form-name &rest args)
  "Render a form"
  (lambda (stream)
    (let ((form
           (getf djula::*template-arguments* form-name)))
      (let ((forms::*form-renderer* :who)
            (forms.who::*html* stream))
        (apply #'forms::render-form-errors form args)))))

(djula:def-tag-compiler :form-row (form-name field-name &rest args)
  (lambda (stream)
    (let ((form
           (getf djula::*template-arguments* form-name)))
      (let ((field-symbol (intern (string-upcase field-name) *template-package*)))
        (let ((field (forms::get-field form field-symbol)))
          (let ((forms::*form-renderer* :who)
                (forms.who::*html* stream))
            (apply #'forms::render-field field form args)))))))

(djula:def-tag-compiler :form-field-label (form-name field-name &rest args)
  (lambda (stream)
    (let ((form
           (getf djula::*template-arguments* form-name)))
      (let ((field-symbol (intern (string-upcase field-name) *template-package*)))
        (let ((field (forms::get-field form field-symbol)))
          (let ((forms::*form-renderer* :who)
                (forms.who::*html* stream))
            (apply #'forms::render-field-label field form args)))))))

(djula:def-tag-compiler :form-field-errors (form-name field-name &rest args)
  (lambda (stream)
    (let ((form
           (getf djula::*template-arguments* form-name)))
      (let ((field-symbol (intern (string-upcase field-name) *template-package*)))
        (let ((field (forms::get-field form field-symbol)))
          (let ((forms::*form-renderer* :who)
                (forms.who::*html* stream))
            (apply #'forms::render-field-errors field form args)))))))

(djula:def-tag-compiler :form-field-widget (form-name field-name &rest args)
  (lambda (stream)
    (let ((form
           (getf djula::*template-arguments* form-name)))
      (let ((field-symbol (intern (string-upcase field-name) *template-package*)))
        (let ((field (forms::get-field form field-symbol)))
          (let ((forms::*form-renderer* :who)
                (forms.who::*html* stream))
            (apply #'forms::render-field-widget field form args)))))))

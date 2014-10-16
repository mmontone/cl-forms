(defpackage :cl-forms.djula
  (:nicknames :forms.djula)
  (:use :cl :forms :djula))

(in-package :forms.djula)

(djula::def-tag-compiler :form (form-name &rest args)
  "Render a form"
  (lambda (stream)
    (let ((form
	   (getf djula::*template-arguments* form-name)))
      (let ((forms::*form-renderer* :who)
	    (forms.who::*html* stream))
	(forms::render-form form)))))

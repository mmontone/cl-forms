(defpackage :cl-forms.who
  (:nicknames :forms.who)
  (:use :cl :forms :who))

(in-package :forms.who)

(defvar *html*)

(defmethod forms::renderer-render-form ((renderer (eql :who)) form)
  (with-html-output-to-string (*html*)
    (:form :action (forms::form-action form)
	   :method (symbol-name (forms::form-method form))
	   (loop for field in (forms::form-fields form)
	      do
		(forms::renderer-render-field renderer (cdr field) form)))))

(defmethod forms::renderer-render-field ((renderer (eql :who)) field form)
  (with-html-output (*html*)
    (:div
     (forms::renderer-render-field-label renderer field form)
     (forms::renderer-render-field-errors renderer field form)
     (forms::renderer-render-field-widget renderer field form))))

(defmethod forms::renderer-render-field-label ((renderer (eql :who)) field form)
  (with-html-output (*html*)
    (:label
     (str (or (forms::field-label field)
	      (forms::field-name field))))))

(defmethod forms::renderer-render-field-label ((renderer (eql :who)) (field forms::submit-form-field) form)
  )

(defmethod forms::renderer-render-field-errors ((renderer (eql :who)) field form)
  )

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (field forms::string-form-field) form)
  (with-html-output (*html*)
    (:input :type "text"
	    :name (forms::form-field-name field form)
	    :placeholder (forms::field-empty-value field)
	    (when (forms::field-value field)
	      (funcall (forms::field-formatter field)
		       (forms::field-value field))))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (field forms::boolean-form-field) form)
  (with-html-output (*html*)
    (:input :type "checkbox"
	    :name (forms::form-field-name field form)
	    :checked (when (forms::field-value field)
		       "checked"))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (field forms::submit-form-field) form)
  (with-html-output (*html*)
    (:input :type "submit"
	    :value (or (forms::field-label field) "Submit"))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (field forms::choice-form-field) form)
  (let ((selected-value (forms::field-key-and-value field)))
    (with-html-output (*html*)
      (:select
       :name (forms::form-field-name field form)
       (loop for (key . choice) in (forms::field-choices-alist field)
	  do
	    (htm
	     (:option :value (princ-to-string key)
		      :selected (when (equalp (first selected-value)
					      key)
				  "selected")
		      (str (funcall (forms::field-formatter field)
					choice)))))))))

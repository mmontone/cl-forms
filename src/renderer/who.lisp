(defpackage :cl-forms.who
  (:nicknames :forms.who)
  (:use :cl :forms :who))

(in-package :forms.who)

(defvar *html*)

(defmethod forms::call-with-form-renderer ((renderer (eql :who)) function)
  (let ((forms::*form-renderer* renderer))
    (with-html-output-to-string (*html*)
      (funcall function))))

(defmethod forms::renderer-render-form ((renderer (eql :who)) form &rest args)
  (with-html-output (*html*)
    (when (forms::form-errors form)
      (htm
       (:ul
	(loop for error in (forms::form-errors form)
	     do
	     (htm (:li (fmt "~A: ~{~A~^, ~}" (first error) (cdr error))))))))
    (:form :action (forms::form-action form)
	   :method (symbol-name (forms::form-method form))
	   (loop for field in (forms::form-fields form)
	      do
		(forms::renderer-render-field renderer (cdr field) form)))))

(defmethod forms::renderer-render-field ((renderer (eql :who)) field form &rest args)
  (with-html-output (*html*)
    (:div
     (forms::renderer-render-field-label renderer field form)
     (forms::renderer-render-field-errors renderer field form)
     (forms::renderer-render-field-widget renderer field form))))

(defmethod forms::renderer-render-field-label ((renderer (eql :who)) field form &rest args)
  (with-html-output (*html*)
    (:label
     (str (or (forms::field-label field)
	      (forms::field-name field))))))

(defmethod forms::renderer-render-field-label ((renderer (eql :who)) (field forms::submit-form-field) form &rest args)
  )

(defmethod forms::renderer-render-field-errors ((renderer (eql :who)) field form &rest args)
  (let ((errors (cdr (assoc (forms::field-name field)
			    (forms::form-errors form)
			    :test #'equalp
			    :key #'string))))
    (when errors
      (with-html-output (*html*)
	(:div :class "errors"
	 (fmt "~{~A~^, ~}" errors))))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (field forms::string-form-field) form &rest args)
  (with-html-output (*html*)
    (:input :type "text"
	    :name (forms::form-field-name field form)
	    :placeholder (forms::field-empty-value field)
	    :value
	    (when (forms::field-value field)
	      (funcall (forms::field-formatter field)
		       (forms::field-value field))))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (field forms::boolean-form-field) form &rest args)
  (with-html-output (*html*)
    (:input :type "checkbox"
	    :name (forms::form-field-name field form)
	    :checked (when (forms::field-value field)
		       "checked"))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (field forms::submit-form-field) form &rest args)
  (with-html-output (*html*)
    (:input :type "submit"
	    :value (or (forms::field-label field) "Submit"))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :who))
     (field forms::choice-form-field) form &rest args)
  (cond
    ((and (forms::field-expanded field)
	  (forms::field-multiple field))
     (let ((selected-keys (mapcar #'first (forms::field-keys-and-values field))))
       ;; Render checkboxes
       (with-html-output (*html*)
	 (loop for (key . choice) in (forms::field-choices-alist field)
	    do
	      (htm
	       (:input :type "checkbox" :name (forms::form-field-name field form)
		       :value key
		       :checked (when (member key selected-keys)
				  "checked")
		       (str (funcall (forms::field-formatter field)
				     choice))))))))
    ((and (forms::field-expanded field)
	  (not (forms::field-multiple field)))
     ;; Render radio buttons
     (let ((selected-value (forms::field-key-and-value field)))
       (with-html-output (*html*)
	 (loop for (key . choice) in (forms::field-choices-alist field)
	    do
	      (htm
	       (:input :type "radio" :name (forms::form-field-name field form)
		       :value (princ-to-string key)
		       :checked (when (equalp (first selected-value)
					      key)
				  "checked")
		       (str (funcall (forms::field-formatter field)
				     choice))))))))
    ((and (not (forms::field-expanded field))
	  (forms::field-multiple field))
     ;; A multiple select box
     (let ((selected-keys (mapcar #'first (forms::field-keys-and-values field))))
       (with-html-output (*html*)
	 (:select
	  :name (forms::form-field-name field form)
	  :multiple "multiple"
	  (loop for (key . choice) in (forms::field-choices-alist field)
	     do
	       (htm
		(:option :value (princ-to-string key)
			 :selected (when (member key selected-keys)
				     "selected")
			 (str (funcall (forms::field-formatter field)
				       choice)))))))))
    ((and (not (forms::field-expanded field))
	  (not (forms::field-multiple field)))
     ;; A single select box
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
				       choice)))))))))))

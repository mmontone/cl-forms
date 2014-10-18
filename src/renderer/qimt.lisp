(defpackage :cl-forms.qimt
  (:nicknames :forms.qimt)
  (:use :cl :forms :qimt :html))

(in-package :forms.qimt)

(defmethod forms::renderer-render-form ((renderer (eql :qimt)) form &rest args)
  (when (forms::form-errors form)
    (<ul
      (loop for error in (forms::form-errors form)
	 do
	   (<li (fxd "~A: ~{~A~^, ~}" (first error) (cdr error))))))
  (<form (<action= (forms::form-action form))
	 (<method= (symbol-name (forms::form-method form)))
	 (loop for field in (forms::form-fields form)
	    do
	      (forms::renderer-render-field renderer (cdr field) form))))

(defmethod forms::renderer-render-field ((renderer (eql :qimt)) field form &rest args)
  (<div
    (forms::renderer-render-field-label renderer field form)
    (forms::renderer-render-field-errors renderer field form)
    (forms::renderer-render-field-widget renderer field form)))

(defmethod forms::renderer-render-field-label ((renderer (eql :qimt)) field form &rest args)
  (<label
    (xd (or (forms::field-label field)
	    (forms::field-name field)))))

(defmethod forms::renderer-render-field-label ((renderer (eql :qimt)) (field forms::submit-form-field) form &rest args)
  )

(defmethod forms::renderer-render-field-errors ((renderer (eql :qimt)) field form &rest args)
  )

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :qimt))
     (field forms::string-form-field) form &rest args)
  (<input (<type= "text")
	  (<name= (forms::form-field-name field form))
	  #+nil(when (forms::field-empty-value field)
		 (<placeholder= (forms::field-empty-value field)))
	  (when (forms::field-value field)
	    (<value=
	     (funcall (forms::field-formatter field)
		      (forms::field-value field))))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :qimt))
     (field forms::boolean-form-field) form &rest args)
  (<input (<type= "checkbox")
	  (<name= (forms::form-field-name field form))
	  (when (forms::field-value field)
	    (<checked= "checked"))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :qimt))
     (field forms::submit-form-field) form &rest args)
  (<input (<type= "submit")
	  (<value= (or (forms::field-label field) "Submit"))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :qimt))
     (field forms::choice-form-field) form &rest args)
  (let ((selected-value (forms::field-key-and-value field)))
    (<select
       (<name= (forms::form-field-name field form))
       (loop for (key . choice) in (forms::field-choices-alist field)
	  do
	    (<option (<value= (princ-to-string key))
		     (when (equalp (first selected-value)
					      key)
		       (<selected= "selected"))
		      (xd (funcall (forms::field-formatter field)
				   choice)))))))

(defmethod forms::renderer-render-field-widget
    ((renderer (eql :qimt))
     (field forms::choice-form-field) form &rest args)
  (cond
    ((and (forms::field-expanded field)
	  (forms::field-multiple field))
     (let ((selected-keys (mapcar #'first (forms::field-keys-and-values field))))
       ;; Render checkboxes
       (loop for (key . choice) in (forms::field-choices-alist field)
	  do
	    (<input (<type= "checkbox")
		    (<name= (forms::form-field-name field form))
		    (<value= key)
		    (when (member key selected-keys)
		      (<checked= "checked"))
		    (xd (funcall (forms::field-formatter field)
				 choice))))))
    ((and (forms::field-expanded field)
	  (not (forms::field-multiple field)))
     ;; Render radio buttons
     (let ((selected-value (forms::field-key-and-value field)))
       (loop for (key . choice) in (forms::field-choices-alist field)
	  do
	    (<input (<type= "radio")
		    (<name= (forms::form-field-name field form))
		    (<value= (princ-to-string key))
		    (when (equalp (first selected-value)
				  key)
		      (<checked= "checked"))
		    (xd (funcall (forms::field-formatter field)
				 choice))))))
    ((and (not (forms::field-expanded field))
	  (forms::field-multiple field))
     ;; A multiple select box
     (let ((selected-keys (mapcar #'first (forms::field-keys-and-values field))))
       (<select
	 (<name= (forms::form-field-name field form))
	 (<multiple= "multiple")
	 (loop for (key . choice) in (forms::field-choices-alist field)
	    do
	      (<option (<value= (princ-to-string key))
		       (when (member key selected-keys)
			 (<selected= "selected"))
		       (xd (funcall (forms::field-formatter field)
				    choice)))))))
    ((and (not (forms::field-expanded field))
	  (not (forms::field-multiple field)))
     ;; A single select box
     (let ((selected-value (forms::field-key-and-value field)))
       (<select
	 (<name= (forms::form-field-name field form))
	 (loop for (key . choice) in (forms::field-choices-alist field)
	    do
	      (<option (<value= (princ-to-string key))
		       (when (equalp (first selected-value)
				     key)
			 (<selected= "selected"))
		       (xd (funcall (forms::field-formatter field)
				    choice)))))))))

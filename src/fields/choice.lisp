(in-package :forms)

(defclass choice-form-field (form-field)
  ((choices :initarg :choices
	    :initform nil
	    :accessor field-choices
	    :documentation "An alist with the choices")
   (preferred-choices :initarg :preferred-choices
		      :initform nil
		      :accessor field-preferred-choices
		      :documentation "If this option is specified, then a sub-set of all of the options will be moved to the top of the select menu.")
   (expanded :initarg :expanded
	     :initform nil
	     :accessor field-expanded
	     :documentation "If set to true, radio buttons or checkboxes will be rendered (depending on the multiple value). If false, a select element will be rendered.")
   (multiple :initarg :multiple
	     :initform nil
	     :accessor field-multiple
	     :documentation "If true, the user will be able to select multiple options (as opposed to choosing just one option). Depending on the value of the expanded option, this will render either a select tag or checkboxes if true and a select tag or radio buttons if false.")
   (key-reader :initarg :key-reader
	       :initform #'identity
	       :accessor field-key-reader
	       :documentation "Function to read the option key from the request")
   (hash-function :initarg :hash-function
		  :initform #'sxhash
		  :accessor field-hash-function
		  :documentation "The function to use for choices key")) 
  (:documentation "A multi-purpose field used to allow the user to \"choose\" one or more options. It can be rendered as a select tag, radio buttons, or checkboxes."))

(defmethod initialize-instance :after ((field choice-form-field) &rest initargs)
  ;; Initialize the key reader and writer
  (let ((choices (field-choices-alist field)))
    (let ((choice-key (first (first choices))))
      (cond
	((stringp choice-key)
	 (setf (field-key-reader field) #'identity))
	((integerp choice-key)
	 (setf (field-key-reader field) #'parse-integer))
	((keywordp choice-key)
	 (setf (field-key-reader field) #'alexandria:make-keyword))
	(t (error "Invalid key ~A" choice-key))))))

(defmethod make-form-field ((field-type (eql :choice)) &rest args)
  (apply #'make-instance 'choice-form-field args))

(defun alistp (alist)
  (and (listp alist)           ; a list
       (every #'consp alist)))

(defmethod field-choices-alist ((field choice-form-field))
  (let ((choices (field-choices field)))
    (if (alistp choices)
	choices
	(mapcar (lambda (choice)
		  (cons (funcall (field-hash-function field)
				 choice)
			choice))
		choices))))

(defmethod field-key-and-value ((field choice-form-field))
  (when (field-value field)
    (if (alistp (field-choices field))
	(find (field-value field)
	      (field-choices field)
	      :key #'cdr
	      :test #'equalp)
	(cons (funcall (field-hash-function field)
		       (field-value field))
	      (field-value field)))))

(defmethod field-keys-and-values ((field choice-form-field))
  (assert (listp (field-value field)))
  (if (alistp (field-value field))
      (field-value field)
      (mapcar (lambda (value)
		(cons (funcall (field-hash-function field)
			       value)
		      value))
	      (field-value field))))

(defmethod field-read-from-request ((field choice-form-field) form)
  (cond
    ((and (field-expanded field)
	  (field-multiple field))
     ;; Checkboxes rendered
     (let ((selected-keys (mapcar (alexandria:compose (field-key-reader field)
						      #'cdr)
				  (remove-if-not (lambda (param)
					   (equalp (first param)
						   (string (form-field-name field form))))
					 (hunchentoot:post-parameters*))))
	   (choices-alist (field-choices-alist field)))
       (setf (field-value field)
	     (mapcar (lambda (key)
		       (cdr (assoc key choices-alist)))
		     selected-keys))))       
    ((and (field-expanded field)
	  (not (field-multiple field)))
     ;; Radio buttons rendered
     (setf (field-value field)
	   (cdr (assoc (funcall (field-key-reader field)
				(hunchentoot:post-parameter (form-field-name field form)))
		       (field-choices-alist field)))))
    ((and (not (field-expanded field))
	  (field-multiple field))
     ;; Multiple select box rendered
     (let ((selected-keys (mapcar (alexandria:compose (field-key-reader field)
						      #'cdr)
				  (remove-if-not (lambda (param)
					   (equalp (first param)
						   (string (form-field-name field form))))
					 (hunchentoot:post-parameters*))))
	   (choices-alist (field-choices-alist field)))
       (setf (field-value field)
	     (mapcar (lambda (key)
		       (cdr (assoc key choices-alist)))
		     selected-keys))))
    ((and (not (field-expanded field))
	  (not (field-multiple field)))
     ;; Single select box
     (setf (field-value field)
	   (cdr (assoc (funcall (field-key-reader field)
				(hunchentoot:post-parameter (form-field-name field form)))
		       (field-choices-alist field)))))))

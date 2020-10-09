(defpackage :cl-forms.peppol
  (:nicknames :forms.peppol)
  (:use :cl :forms)
  (:export
   :amount-form-field
   :country-form-field
   :currency-form-field))

(in-package :forms.peppol)

(defun round-amount (amount &optional (divisor 1))
  "Amount is in ører"
  (multiple-value-bind (quotient remainder) (truncate (/ amount divisor))
    (if (>= (abs remainder) 1/2)
        (+ quotient (truncate (signum remainder)))
        quotient)))

(defun format-amount (amount &optional (decimals 2))
  "Format an amount"
  (multiple-value-bind (kroner orer) (truncate amount (expt 10 decimals))
    ;; (code-char 160) is no break space (aka &nbsp;)
    ;;(format nil "~:,,v,3D,~2,'0D" (code-char 160) kroner (abs orer))
    (format nil "~a,~a" kroner (abs orer))
    ))

(defun parse-amount (string &optional (decimals 2))
  "Parse an amount"
  (setf string (remove (code-char 160) string))
  (setf string (remove #\Space string))
  (setf string (substitute #\. #\, string))
  (let ((decs (or (position #\. (reverse string)) 0)))
    (round-amount (* (expt 10 decimals)
                     (/ (parse-integer (remove #\. string))
                        (expt 10 decs))))))

(defun decimal (number &optional (decimals 2))
  (* number (expt 10 decimals)))

(defun float-to-decimal (float &optional (decimals 2))
  (format-amount (format nil (format nil "~~~a$" decimals) float) decimals))

;;;; ** Amount form field

(defclass amount-form-field (forms::integer-form-field)
  ()
  (:default-initargs
   :formatter (lambda (value stream)
                (princ (format-amount value) stream))))

(defmethod forms::field-read-from-request ((field amount-form-field) form parameters)
  (setf (forms::field-value field)
        (let ((value
                (cdr (assoc (forms::field-request-name field form) parameters :test #'string=))))
          (and value (parse-amount value)))))

;; (defmethod forms::validate-form-field ((field amount-form-field))
;;   (let ((valid-p (validate-amount (forms::field-value field) :error-p nil)))
;;     (multiple-value-bind (valid-constraints-p errors)
;;         (call-next-method)
;;       (values (and valid-p valid-constraints-p)
;;               (if (not valid-p) (cons "Amount is invalid" errors)
;;                   errors)))))

(defmethod forms::make-form-field ((field-type (eql :amount)) &rest args)
  (apply #'make-instance 'amount-form-field args))

;;;; *** Currency form field

(defclass currency-form-field (forms::choice-form-field)
  ()
  (:default-initargs
   :use-key-as-value t
   :choices (mapcar (lambda (x)
                      (cons (cdr x) (car x)))
                    peppol/code-lists::|ISO 4217 Currency codes|)
   :test (lambda (key key-and-value)
           (string= key (car key-and-value)))))

(defmethod forms::make-form-field ((field-type (eql :currency)) &rest args)
  (apply #'make-instance 'currency-form-field args))

;;;; *** Country form field

(defclass country-form-field (forms::choice-form-field)
  ()
  (:default-initargs
   :use-key-as-value t
   :choices (mapcar (lambda (x)
                      (cons (cdr x) (car x)))
                    peppol/code-lists::|ISO 3166-1:Alpha2 Country codes|)
   :test (lambda (key key-and-value)
           (string= key (car key-and-value)))))

(defmethod forms::make-form-field ((field-type (eql :country)) &rest args)
  (apply #'make-instance 'country-form-field args))

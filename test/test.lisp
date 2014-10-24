(in-package :forms.test)

(defun run-tests ()
  (run 'forms-test))

(def-suite forms-test :description "Forms tests")

(in-suite forms-test)

(test string-field-test
  (let ((field (make-instance 'forms::string-form-field :name 'foo
			      :value "a")))
    (is (forms::validate-form-field field))
    (setf (forms::field-value field) 2)
    (is (not (forms::validate-form-field field)))))

(test boolean-field-test
  (let ((field (make-instance 'forms::boolean-form-field :name 'foo
			      :value t)))
    (is (forms::validate-form-field field))
    (setf (forms::field-value field) 33)
    (is (not (forms::validate-form-field field)))
    (setf (forms::field-value field) "bar")
    (is (not (forms::validate-form-field field)))
    (setf (forms::field-value field) t)
    (is (forms::validate-form-field field))))
    


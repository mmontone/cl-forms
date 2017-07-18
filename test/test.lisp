(in-package :forms.test)

(defun run-tests ()
  (run 'forms-test))

(def-suite forms-test :description "Forms tests")

(in-suite forms-test)

(test string-field-test
  (let ((field (make-instance 'forms::string-form-field
                              :name 'foo
                              :value "a")))
    (is (forms::validate-form-field field))
    (setf (forms::field-value field) 2)
    (is (not (forms::validate-form-field field)))
    (setf (forms::field-value field) nil)
    (is (not (forms::validate-form-field field))))
  (let ((field (make-instance 'forms::string-form-field
                              :name 'foo
                              :value nil
                              :required-p nil)))
    (is (forms::validate-form-field field))
    (setf (forms::field-value field) 2)
    (is (not (forms::validate-form-field field)))
    (setf (forms::field-value field) "A")
    (is (forms::validate-form-field field))))

(test boolean-field-test
  (let ((field (make-instance 'forms::boolean-form-field
                              :name 'foo
                              :value t)))
    (is (forms::validate-form-field field))
    (setf (forms::field-value field) 33)
    (is (not (forms::validate-form-field field)))
    (setf (forms::field-value field) "bar")
    (is (not (forms::validate-form-field field)))
    (setf (forms::field-value field) t)
    (is (forms::validate-form-field field))))

(test integer-field-test
  (let ((field (make-instance 'forms::integer-form-field :name 'foo
                              :value 2)))
    (is (forms::validate-form-field field))
    (setf (forms::field-value field) nil)
    (is (not (forms::validate-form-field field)))
    (setf (forms::field-value field) "22")
    (is (not (forms::validate-form-field field))))
  (let ((field (make-instance 'forms::integer-form-field
                              :name 'foo
                              :value nil
                              :required-p nil)))
    (is (forms::validate-form-field field))
    (setf (forms::field-value field) 2)
    (is (forms::validate-form-field field))
    (setf (forms::field-value field) "A")
    (is (not (forms::validate-form-field field)))))

(test choice-field-test
  (let ((field (make-instance 'forms::choice-form-field
                              :name 'foo
                              :choices (list "foo" "bar")
                              :test #'equalp
                              :value "foo")))
    (is (forms::validate-form-field field))
    (setf (forms::field-value field) nil)
    (is (not (forms::validate-form-field field)))
    (setf (forms::field-value field) "baz")
    (is (not (forms::validate-form-field field)))
    (setf (forms::field-value field) "bar")
    (is (forms::validate-form-field field)))
  (let ((field (make-instance 'forms::choice-form-field
                              :name 'foo
                              :choices (list "foo" "bar")
                              :test #'equalp
                              :required-p nil
                              :value nil)))
    (is (forms::validate-form-field field))
    (setf (forms::field-value field) (list "foo"))
    (is (not (forms::validate-form-field field)))
    (setf (forms::field-value field) "foo")
    (is (forms::validate-form-field field)))
  (let ((field (make-instance 'forms::choice-form-field
                              :name 'foo
                              :choices (list "foo" "bar")
                              :test #'equalp
                              :multiple t
                              :value nil)))
    (is (not (forms::validate-form-field field)))
    (setf (forms::field-value field) "foo")
    (is (not (forms::validate-form-field field)))
    (setf (forms::field-value field) (list "foo"))
    (is (forms::validate-form-field field))
    (setf (forms::field-value field) (list "foo" "baz"))
    (is (not (forms::validate-form-field field)))))

;; (forms:defform subform1 ()
;;   ((foo :string :value "")
;;    (bar :number :value 1)))

;; (forms:defform subform2 ()
;;   ((sex :choice :choices (list "Male" "Female") :value "Male")))

;; (forms:defform subform-test-form ()
;;   ((subform1 :subform :subform 'subform1)
;;    (subform2 :subform :subform 'subform2)))

;; (test subform-field-test
;;   (let ((form (get-form 'subform-test-form)))
;;     (
;;   (let ((field

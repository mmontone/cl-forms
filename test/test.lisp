(in-package :cl-forms.test)

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


(defclass mock-request ()
  ((post-parameters :initarg :post-parameters
		    :accessor forms::request-post-parameters)))

(forms:defform test-form-1 ()
  ((foo :string :value "")
   (bar :integer :value 1)
   (baz :string :value "" :html-name "bazz")
   (choice :choice :choices (list "foo" "bar" "baz")
		   :test 'string=
		   :hash-function 'identity
		   :use-key-as-value t
		   :required-p nil)))

(forms:defform subform1 ()
  ((foo :string :value "")
   (bar :integer :value 1)
   (baz :string :value "" :html-name "bazz")))

(forms:defform subform2 ()
  ((sex :choice :choices (list "Male" "Female") :value "Male")))

(forms:defform subform-test-form ()
  ((subform1 :subform :subform 'subform1)
   (subform2 :subform :subform 'subform2)))

(test form-test-1 ()
  (let* ((form (find-form 'test-form-1))
	 (request (make-instance 'mock-request
				 :post-parameters '(("foo" . "foo")
						    ("bar" . "22")
						    ("choice" . "")
                                                    ("bazz" . "baz")))))
    (handle-request form request)
    (is-true (validate-form form))
    (with-form-field-values (foo bar baz) form
      (is (equalp (list foo bar baz)
                  (list "foo" 22 "baz")))))

  (let* ((form (find-form 'test-form-1))
	 (request (make-instance 'mock-request
				 :post-parameters '(("foo" . "foo")
						    ("bar" . "bar")
						    ("choice" . "")
                                                    ("bazz" . "baz")))))
    (handle-request form request)
    (is-false (validate-form form)))

  (let* ((form (find-form 'test-form-1))
	 (request (make-instance 'mock-request
				 :post-parameters '(("foo" . "foo")
						    ("bar" . "44")
						    ("choice" . "badchoice")
                                                    ("bazz" . "baz")))))
    (handle-request form request)
    (is-false (validate-form form)))

  (let* ((form (find-form 'test-form-1))
	 (request (make-instance 'mock-request
				 :post-parameters '(("foo" . "foo")
						    ("bar" . "44")
						    ("choice" . "foo")
                                                    ("bazz" . "baz")))))
    (handle-request form request)
    (is-true (validate-form form))))

(defform string-choice-test-form ()
  ((choice :string-choice :choices '("foo" "bar" "baz"))))

(test string-choice-test
  (let* ((form (find-form 'string-choice-test-form))
	 (request (make-instance 'mock-request
				 :post-parameters '(("foo" . "foo")
						    ("bar" . "44")
						    ("choice" . "badchoice")))))
    (handle-request form request)
    (is-false (validate-form form)))

  (let* ((form (find-form 'string-choice-test-form))
	 (request (make-instance 'mock-request
				 :post-parameters '(("foo" . "foo")
						    ("bar" . "44")
						    ("choice" . "foo")))))
    (handle-request form request)
    (is-true (validate-form form))))

(defform list-test-form ()
  ((list :list :type :string)))

(test list-field-test
  (let* ((form (find-form 'list-test-form))
	 (request (make-instance 'mock-request
				 :post-parameters '(("list[0]" . "foo")
						    ("list[1]" . "bar")
						    ("list[2]" . "baz")))))
    (handle-request form request)
    (with-form-field-values (list) form
      (is (equalp (list "foo" "bar" "baz") list)))))

(test subform-field-test
  
  )

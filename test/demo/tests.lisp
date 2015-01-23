(in-package :forms.test)

(hunchentoot:define-easy-handler (tests-demo-handler :uri "/tests") ()
  (render-demo-page :demo #'tests-demo
		    :source (asdf:system-relative-pathname :cl-forms.demo 
							   "test/demo/tests.lisp")
		    :active-menu :tests))

(defun tests-demo ()
  (let ((test-results (forms.test:run-tests)))
    (who:with-html-output (*html*)
      (:ul
	(loop for test-result in test-results
	   do
	     (who:htm
	      (:li
		(if (fiveam::test-passed-p test-result)
		    (who:htm (who:str "Passed!!"))
		    (who:htm (who:str "Failed!!"))))))))))

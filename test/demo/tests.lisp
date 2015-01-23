(in-package :forms.test)

(hunchentoot:define-easy-handler (tests-demo-handler :uri "/tests") ()
  (render-demo-page :demo #'tests-demo
		    :source (asdf:system-relative-pathname :cl-forms.demo 
							   "test/demo/tests.lisp")
		    :active-menu :tests))

(defun tests-demo ()
  (let ((test-results (forms.test:run-tests)))
    (who:with-html-output (forms.who::*html*)
      (:div :class "panel-group" :id "accordion" :role "tablist" 
	    :aria-multiselectable"true"
	    (loop for test-result in test-results
               for i = 1 then (1+ i)
	       do
		 (who:htm
		  (:div :class "panel panel-default"
			(:div :class "panel-heading" :role "tab" :id "headingOne"
			      (:h4 :class "panel-title"
				   (:a :data-toggle "collapse" :data-parent "#accordion" :href (format nil "#collapse~A" i)
				       :aria-expanded "false"
				       :aria-controls (format nil "collapse~A" i)
				       (who:str (prin1-to-string (5am::name (5am::test-case test-result))))
				       (who:str ": ")
				       (who:str (if (5am::test-passed-p test-result)
						    "Passed!!"
						    "Failed!!"))))))
		  (:div :id (format nil "collapse~A" i) 
			:class "panel-collapse collapse" :role "tabpanel" :aria-labelledby (format nil "heading~A" i)
			(:div :class "panel-body"
			      (who:str (prin1-to-string (fiveam::test-expr test-result)))))))))))

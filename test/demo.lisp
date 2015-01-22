(in-package :forms.test)

(defparameter *demo-acceptor*
  (make-instance 'hunchentoot:easy-acceptor :port 9090))

(push
 (hunchentoot:create-static-file-dispatcher-and-handler
  "/bootstrap.css"
  (asdf:system-relative-pathname :cl-forms
                                 "test/static/bower_components/bootstrap/dist/css/bootstrap.min.css"))
 hunchentoot:*dispatch-table*)

(push
 (hunchentoot:create-static-file-dispatcher-and-handler
  "/bootstrap.js"
  (asdf:system-relative-pathname :cl-forms
                                 "test/static/bower_components/bootstrap/dist/js/bootstrap.min.js"))
 hunchentoot:*dispatch-table*)

(push
 (hunchentoot:create-static-file-dispatcher-and-handler
  "/jquery.js"
  (asdf:system-relative-pathname :cl-forms
				 "test/static/bower_components/jquery/dist/jquery.min.js"))
 hunchentoot:*dispatch-table*)

(defun start-demo ()
  (hunchentoot:start *demo-acceptor*))

(defun stop-demo ()
  (hunchentoot:stop *demo-acceptor*))

(defparameter *html* nil)

(defun call-with-demo-page (function)
  (who:with-html-output-to-string (*html*)
    (:html
     (:head
      (:title "cl-forms demo")
      (:link :rel "stylesheet" :href "bootstrap.css"))
     (:body
      (funcall function)
      ;(:script :src "https://google-code-prettify.googlecode.com/svn/loader/run_prettify.js")
      (:script :src "jquery.js")
      (:script :src "bootstrap.js")))))

(defmacro with-demo-page (&body body)
  `(call-with-demo-page (lambda () ,@body)))

(hunchentoot:define-easy-handler (demo :uri "/") ()
  (with-demo-page
    (who:with-html-output (*html*)
      (:header :role"banner" :id "top" :class "navbar navbar-static-top"
	       (:div :class "container"
		     (:div :class "navbar-header"
			   (:a :class "navbar-brand" :href "/" (who:str "CL-FORMS demo")))))
      (:div :class "container"
	    (:div :class "col-md-3"
		  (:ul :class "nav nav-pills nav-stacked"
		       (:li :role "presentation" :class "active"
			    (:a :href "/fields" (who:str "Fields")))
		       (:li :role "presentation"
			    (:a :href "/validation" (who:str "Validation")))
		       (:li :role "presentation"
			    (:a :href "/themes" (who:str "Themes")))
		       (:li :role "presentation"
			    (:a :href "/renderers" (who:str "Renderers")))
		       (:li :role "presentation"
			    (:a :href "/tests" (who:str "Tests")))))
	    (:div :role "tabpanel" :class "col-md-9"
		  (:ul :class "nav nav-tabs" :role "tablist"
		       :data-toggle "tab"
		       (:li :role "tab" :class "active"
			    :aria-controls "demo"
			    (:a :href "#demo" (who:str "Demo")))
		       (:li :role "tab"
			    :aria-controls "source"
			    (:a :href "#source" (who:str "Source"))))
		  (:div :class "tab-content"
			(:div :role "tabpanel" :class "tab-pane active" :id "demo"
			      (:p (who:str "Demo")))
			(:div :role "tabpanel" :class "tab-pane" :id "source"
			      (:p (who:str "Source")))))
	    ))))

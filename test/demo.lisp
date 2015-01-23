(in-package :forms.test)

(defparameter *html* nil)

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))


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

(defun render-demo-page (&key (demo (error "Provide the demo"))
                              (source (error "Provide the source"))
                              (active-menu :fields))
  (who:with-html-output-to-string (*html*)
    (:html
     (:head
      (:title "cl-forms demo")
      (:link :rel "stylesheet" :href "bootstrap.css"))
     (:body
      (:header :role"banner" :id "top" :class "navbar navbar-static-top"
               (:div :class "container"
                     (:div :class "navbar-header"
                           (:a :class "navbar-brand" :href "/" (who:str "CL-FORMS demo")))))
      (:div :class "container"
            (:div :class "col-md-3"
                  (:ul :class "nav nav-pills nav-stacked"
                       (:li :role "presentation"
                            :class (and (equalp active-menu :fields)
                                        "active")
                            (:a :href "/fields" (who:str "Fields")))
                       (:li :role "presentation"
                            :class (and (equalp active-menu :validation)
                                        "active")
                            (:a :href "/validation" (who:str "Validation")))
                       (:li :role "presentation"
                            :class (and (equalp active-menu :themes)
                                        "active")
                            (:a :href "/themes" (who:str "Themes")))
                       (:li :role "presentation"
                            :class (and (equalp active-menu :renderers)
                                        "active")
                            (:a :href "/renderers" (who:str "Renderers")))
                       (:li :role "presentation"
                            :class (and (equalp active-menu :tests)
                                        "active")
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
                              (funcall demo))
                        (:div :role "tabpanel" :class "tab-pane" :id "source"
                              (:div :class "prettyprint"
				    (file-string source))))))
                                        ;(:script :src "https://google-code-prettify.googlecode.com/svn/loader/run_prettify.js")
      (:script :src "jquery.js")
      (:script :src "bootstrap.js")))))

(hunchentoot:define-easy-handler (demo-main :uri "/") ()
  (render-demo-page :demo #'fields-demo
		    :source (asdf:system-relative-pathname :cl-forms.demo 
							   "test/demo/fields.lisp")
		    :active-menu :fields))

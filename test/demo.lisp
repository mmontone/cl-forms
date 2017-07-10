(in-package :forms.test)

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun ensure-file (path)
  (when (not (probe-file path))
    (error "File not found: ~A" path))
  path)

(defparameter *demo-acceptor*
  (make-instance 'hunchentoot:easy-acceptor :port 9090))

(defun define-static-resource (uri relative-path)
  (push
   (hunchentoot:create-static-file-dispatcher-and-handler
    uri
    (ensure-file
     (asdf:system-relative-pathname :cl-forms relative-path)))
   hunchentoot:*dispatch-table*)
  uri)

(define-static-resource "/bootstrap.css"
    "test/static/bower_components/bootstrap/dist/css/bootstrap.min.css")

(define-static-resource "/bootstrap.js"
    "test/static/bower_components/bootstrap/dist/js/bootstrap.min.js")

(define-static-resource "/jquery.js"
    "test/static/bower_components/jquery/dist/jquery.min.js")

(define-static-resource "/prettify.js"
    "test/static/prettify/prettify.js")

(define-static-resource "/lang-lisp.js"
    "test/static/prettify/lang-lisp.js")

(define-static-resource "/prettify.css"
    "test/static/prettify/prettify.css")

(define-static-resource "/parsley.css"
    "test/static/bower_components/parsleyjs/src/parsley.css")

(define-static-resource "/parsley.js"
    "test/static/bower_components/parsleyjs/dist/parsley.js")


(defun run-demo ()
  (hunchentoot:start *demo-acceptor*))

(defun stop-demo ()
  (hunchentoot:stop *demo-acceptor*))

(defun render-demo-page (&key (demo (error "Provide the demo"))
                           (source (error "Provide the source"))
                           (active-menu :fields))
  (who:with-html-output-to-string (forms.who::*html*)
    (:html
     (:head
      (:title "cl-forms demo")
      (:link :rel "stylesheet" :href "/bootstrap.css")
      (:link :rel "stylesheet" :href "/prettify.css")
      (:link :rel "stylesheet" :href "/parsley.css")
      (:script :src "/jquery.js")
      (:script :src "/bootstrap.js")
      (:script :src "/prettify.js")
      (:script :src "/lang-lisp.js")
      (:script :src "/parsley.js"))
     (:style
      (who:str
       (cl-css:css '(("ul.errors li" :color "red")
                     (".has-error input" :background-color "pink")))))       
     (:body :onload "prettyPrint()"
            (:header :role"banner" :id "top" :class "navbar navbar-static-top"
                     (:div :class "container"
                           (:div :class "navbar-header"
                                 (:a :class "navbar-brand"
                                     :href "/" (who:str "CL-FORMS demo")))))
            (:div :class "container"
                  (:div :class "col-md-3"
                        (:ul :class "nav nav-pills nav-stacked"
                             (:li :role "presentation"
                                  :class (and (equalp active-menu :fields)
                                              "active")
                                  (:a :href "/fields" (who:str "Fields")))
                             (:li :role "presentation"
                                  :class (and (equalp active-menu :models)
                                              "active")
                                  (:a :href "/models" (who:str "Models")))
                             (:li :role "presentation"
                                  :class (and (equalp active-menu :validation)
                                              "active")
                                  (:a :href "/validation" (who:str "Validation")))
                             (:li :role "presentation"
                                  :class (and (equalp active-menu :client-validation)
                                              "active")
                                  (:a :href "/client-validation" (who:str "Client validation")))
                             (:li :role "presentation"
                                  :class (and (equalp active-menu :layout)
                                              "active")
                                  (:a :href "/layout" (who:str "Layout")))
                             (:li :role "presentation"
                                  :class (and (equalp active-menu :themes)
                                              "active")
                                  (:a :href "/themes" (who:str "Themes")))
                             (:li :role "presentation"
                                  :class (and (equalp active-menu :renderers)
                                              "active")
                                  (:a :href "/renderers" (who:str "Renderers")))
                             (:li :role "presentation"
                                  :class (and (equalp active-menu :composition)
                                              "active")
                                  (:a :href "/composition" (who:str "Composition")))
                             (:li :role "presentation"
                                  :class (and (equalp active-menu :tests)
                                              "active")
                                  (:a :href "/tests" (who:str "Tests")))))
                  (:div :role "tabpanel" :class "col-md-9"
                        (:ul :class "nav nav-tabs" :role "tablist"
                             (:li :role "tab" :class "active"
                                  :aria-controls "demo"
                                  (:a :href "#demo" :data-toggle "tab"
                                      (who:str "Demo")))
                             (:li :role "tab"
                                  :aria-controls "source"
                                  (:a :href "#source" :data-toggle "tab"
                                      (who:str "Source"))))
                        (:div :class "tab-content"
                              (:div :role "tabpanel" :class "tab-pane active"
                                    :id "demo"
                                    (funcall demo))
                              (:div :role "tabpanel" :class "tab-pane"
                                    :id "source"
                                    (:pre :class "prettyprint"
                                          (who:str
                                           (file-string source)
                                           ))))))
            ))))

(hunchentoot:define-easy-handler (demo-main :uri "/") ()
  (render-demo-page :demo #'fields-demo
                    :source (asdf:system-relative-pathname :cl-forms.demo
                                                           "test/demo/fields.lisp")
                    :active-menu :fields))

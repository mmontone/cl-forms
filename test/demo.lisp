(in-package :forms.test)

(defparameter *demo-acceptor*
  (make-instance 'hunchentoot:easy-acceptor :port 9090))

(push
 (hunchentoot:create-static-file-dispatcher-and-handler
  "/bootstrap.css"
  (asdf:system-relative-pathname :cl-forms
                                 "test/static/bower_components/bootstrap/dist/css/boostrap.min.css"))
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

(hunchentoot:define-easy-handler (demo :uri "/") ()
  (who:with-html-output-to-string (*html*)
    (:p (who:str "hello"))))

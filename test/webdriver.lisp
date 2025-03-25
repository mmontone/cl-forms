(require :cl-webdriver-client)
(require :cl-forms)
(require :hunchentoot)
(require :cl-forms.who)
(require :cl-json)
(require :access)

(defpackage :cl-forms.webdriver-test
  (:use :cl))

(in-package :cl-forms.webdriver-test)

(forms:defform webdriver-test-form (:action "/" :method :post)
  ((name :string)
   (age :integer)
   (submit :submit)))

(forms:find-form 'webdriver-test-form)

(hunchentoot:define-easy-handler (webdriver-test-handler :uri "/")
    ()
  (case (hunchentoot:request-method*)
    (:get
     (who:with-html-output-to-string (html)
       (:html
        (:head
         (:title (who:str "CL-FORMS Webdriver test")))
        (:body
         (let ((form (forms:find-form 'webdriver-test-form)))
           (forms:set-field-value form 'name "Mariano")
           (forms:set-field-value form 'age 41)
           (forms:with-form-renderer :who
             (let ((forms.who:*html* html))
               (forms:render-form form))))))))
    (:post
     (let ((form (forms:find-form 'webdriver-test-form)))
       (forms:handle-request form)
       (forms:with-form-field-values (name age) form
         (who:with-html-output-to-string (html)
           (:html
            (:head
             (:title (who:str "CL-FORMS Webdriver test")))
            (:body
             (:textarea :id "result"
                        (who:fmt "~a" (and (equalp name "Mariano")
                                           (equalp age 41))))))))))
    (t (hunchentoot:abort-request-handler))))

(defvar *acceptor*)

(defun start (&optional (port 0))
  (setf *acceptor* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port))))

(defun stop ()
  (hunchentoot:stop *acceptor*)
  (setf *acceptor* nil))

;; (start)

(defun test ()
  "Returns T on success."
  (webdriver:with-session ()
    (setf (webdriver:url) (format nil "http://localhost:~a" (hunchentoot:acceptor-port *acceptor*)))
    (let ((submit (webdriver:find-element "input[type=\"submit\"]")))
      (webdriver:element-click submit))
    (let ((result (webdriver:element-text (webdriver:find-element "#result"))))
      (equalp result "T"))))

;; (test)



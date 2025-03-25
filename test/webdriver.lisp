(require :cl-webdriver-client)
(require :cl-forms)
(require :hunchentoot)
(require :cl-forms.who)
(require :cl-json)
(require :access)
(require :generic-cl)

(defpackage :cl-forms.webdriver-test
  (:local-nicknames
   (:acc :access)
   (:gcl :generic-cl))
  (:use :cl))

(in-package :cl-forms.webdriver-test)

(defmethod access:do-access ((form forms:form) (field-name symbol) &key test key type skip-call?)
  (declare (ignore test key type skip-call?))
  (forms:get-field-value form field-name))

(defmethod gcl:equalp ((f1 forms:form) (f2 forms:form))
  (every (lambda (args)
           (apply #'gcl:equalp args))
         (mapcar #'list
                 (forms:form-fields f1)
                 (forms:form-fields f2))))

(defmethod gcl:equalp ((f1 forms:form-field) (f2 forms:form-field))
  (and (equalp (forms::field-name f1)
               (forms::field-name f2))
       (equalp (forms::field-value f1)
               (forms::field-value f2))))

(forms:defform attachment-form ()
  ((document-name :string)
   (document-type :string-choice :choices '("pdf" "odt" "docx" "txt"))))

(forms:defform webdriver-test-form (:action "/" :method :post)
  ((name :string)
   (age :integer)
   (url :url)
   (gender :string-choice :choices '("Male" "Female"))
   (attachment :subform :subform 'attachment-form)
   (attachments :list :type '(:subform :subform attachment-form)
                      :empty-item-predicate (lambda (attachment)
                                              (str:emptyp (access:access attachment 'document-name))))
   (submit :submit)))

(forms:find-form 'webdriver-test-form)

(defun make-attachment (doc-name doc-type)
  (let ((attachment (forms:find-form 'attachment-form)))
    (forms:set-field-value attachment 'document-name doc-name)
    (forms:set-field-value attachment 'document-type doc-type)
    attachment))

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
           (forms:set-field-value form 'url "http://common-lisp.net")
           (forms:set-field-value form 'gender "Male")
           (forms:set-field-value form 'attachment (make-attachment "foo.odt" "odt"))
           (forms:set-field-value form 'attachments
                                  (list (make-attachment "foo.odt" "odt")
                                        (make-attachment "bar.pdf" "pdf")
                                        (make-attachment "baz.txt" "txt")))
           (forms:with-form-renderer :who
             (let ((forms.who:*html* html))
               (forms:render-form form))))))))
    (:post
     (let ((form (forms:find-form 'webdriver-test-form)))
       (forms:handle-request form)
       (forms:with-form-field-values (name age gender url attachment attachments) form
         (who:with-html-output-to-string (html)
           (:html
            (:head
             (:title (who:str "CL-FORMS Webdriver test")))
            (:body
             (:textarea :id "result"
                        (who:fmt "~a" (and (equalp name "Mariano")
                                           (equalp age 41)
                                           (equalp gender "Male")
                                           (equalp url "http://common-lisp.net")
                                           (equalp (forms:get-field-value attachment 'document-name) "foo.odt")
                                           (equalp (forms:get-field-value attachment 'document-type) "odt")
                                           (= (length attachments) 3)
                                           (gcl:equalp attachments
                                                       (list (make-attachment "foo.odt" "odt")
                                                             (make-attachment "bar.pdf" "pdf")
                                                             (make-attachment "baz.txt" "txt")))
                                           )))))))))
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

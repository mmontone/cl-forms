(in-package :cl-forms)

(defform my-form ()
  ((name string
	 :accessor person-name
	 :validator (clavier:is-a-string))
   (sex select
	:options 

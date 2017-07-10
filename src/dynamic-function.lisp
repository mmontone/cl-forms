(in-package :cl-forms)

;; Implementation of dynamically scoped functions

(defun make-dfun-varname (fname)
  (intern (format nil "*DFUN-~A*" fname)
          (symbol-package fname)))

(defmacro dflet (fbindings &body body)
  (let ((nextfun (gensym)))
    `(let
         ,(loop for fbinding in fbindings
             collect
               (destructuring-bind (fname args &body body) fbinding
                 (let ((dfun-varname (make-dfun-varname fname)))
                   `(,dfun-varname (cons
                                    (let ((,nextfun (first ,dfun-varname)))
                                      (lambda ,args
                                        (flet ((call-next-function ()
                                                 (apply ,nextfun (list ,@args))))
                                          ,@body)))
                                    ,dfun-varname)))))
       ,@body)))

(defmacro defdfun (name args &body body)
  (let ((dfun-varname (make-dfun-varname name)))
    `(progn
       (defparameter ,dfun-varname
         (list (lambda ,args
                 ,@body)))
       (setf (symbol-function ',name)
             (lambda ,args
               (apply (first ,dfun-varname) (list ,@args)))))))

;; Example

#+example(progn
           (defdfun dfoo ()
             (print "foo1"))

           (dfoo)

           (defun test-dfoo ()
             (dfoo))

           (progn
             (test-dfoo)
             (dflet ((dfoo ()
                           (print "foo2")))
               (test-dfoo))
             (test-dfoo))

           (progn
             (test-dfoo)
             (dflet ((dfoo ()
                           (print "foo2")
                           (call-next-function)))
               (test-dfoo))
             (test-dfoo))
           )

;; Cached fib via dynamic functions

;; (defdfun fib (x)
;;   (case x
;;     (0 1)
;;     (1 1)
;;     (t (+ (fib (- x 1)) (fib (- x 2))))))

;; (fib 22)

;; (defun memo-fib (x)
;;   (let (memo)
;;     (dflet ((fib (x) (let ((result (cdr (assoc x memo))))
;;                      (if result
;;                          (prog1 result
;;                            (format t "~& found (fib ~A) => ~A" x result))
;;                          (prog1 (setf result (call-next-function))
;;                            (format t "~& called (fib ~A) => ~A" x result)
;;                            (setf memo (acons x result memo)))))))
;;       (fib x))))

;; (time (fib 40))
;; (time (memo-fib 40))

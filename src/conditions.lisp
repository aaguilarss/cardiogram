;; This file is part of cardiogram
;; (c) 2018 - Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/conditions
  (:use :cl)
  (:export :test-failure :test-dependencies-error
           :undefined-test :undefined-test-name
           :*ignore-test-errors* :*ignore-errors*)
  (:export :read-substitute-expr))
(in-package :cardiogram/conditions)



(defparameter *test-error* *error-output*)
(defparameter *ignore-test-errors* nil)
(defparameter *ignore-errors* nil)


;; Regarding tests

(define-condition undefined-test (error)
  ((name
     :initarg :name
     :accessor undefined-test-name))
  (:report (lambda (c s)
             (let ((n (undefined-test-name c)))
               (format s "The test ~a is undefined. ~:[~;But ~a is fbound.~]"
                       n (fboundp n) n)))))

(define-condition test-failure (error)
  ((name
     :initarg :name
     :accessor test-failure-test-name)))

(define-condition test-dependencies-error (test-failure) ()
  (:report
    (lambda (c s)
      (format s "The test ~a fails and causes a dependency failure."
              (test-failure-test-name c))))
  (:documentation "To be signaled when checking dependencies"))


(define-condition test-time-limit-exceeded (test-failure) ()
  (:documentation "To be signaled when checking dependencies"))




;; Regarding the definition of tests

(define-condition unknown-test-in-combination (error) ()
  (:documentation "To be signaled when resolving a
                  test's combination"))

(define-condition unknown-test-in-dependency-expr (error) ()
  (:documentation "To be signaled when checking dependency expr."))


;; This file is part of cardiogram
;; (c) 2018 - Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/conditions
  (:use :cl)
  (:export :test-failure :test-dependencies-error
           :undefined-test :undefined-test-name
           :undefined-test-in-dependency-of :dependency-name
           :undefined-test-in-combination :combination-test-name
           :*ignore-test-errors* :*ignore-errors*)
  (:export :read-substitute-expr))
(in-package :cardiogram/conditions)


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
     :accessor test-failure-test-name))
  (:report (lambda (c s)
             (format s "Test ~a failed. See *test-output* for report"
                     (test-failure-test-name c)))))

(define-condition test-dependencies-error (test-failure) ()
  (:report
    (lambda (c s)
      (format s "The test ~a fails and causes a dependency failure."
              (test-failure-test-name c))))
  (:documentation "To be signaled when checking dependencies"))

(define-condition undefined-test-in-combination (undefined-test)
  ((combination-test
     :initarg :combination-test
     :accessor combination-test-name))
  (:report (lambda (c s)
             (let ((n (undefined-test-name c)))
               (format s "When resolving the combination for test ~a the test ~a is undefined ~:[~;But ~a is fbound.~]"
                       (combination-test-name c) n (fboundp n) n)))))

(define-condition undefined-test-in-dependency-of (undefined-test)
  ((dependency-name
     :initarg :dependency-name
     :accessor dependency-name))
  (:report (lambda (c s)
             (let ((n (undefined-test-name c)))
               (format s "When adding ~a as a dependency, the test ~a is undefined ~:[~;But ~a is fbound.~]"
                       (dependency-name c) n (fboundp n) n)))))

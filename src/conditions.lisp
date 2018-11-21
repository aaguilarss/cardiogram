;; This file is part of cardiogram
;; (c) 2018 - Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/conditions
  (:use :cl))
(in-package :cardiogram/conditions)



(defparameter *test-error* *error-output*)


(define-condition test-failure (error)
  ((text
     :initarg :text
     :accessor test-failure-error-text)))

(define-condition test-dependencies-error (test-failure) ())

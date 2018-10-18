;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)
(defpackage :cardio/clauses
  (:use :cl
        :cardio/verdicts))
(in-package :cardio/clauses)
(annot:enable-annot-syntax)



(defmacro is (expected form &optional &key description skip)
  `(let* ((source (format t "~a.~a" test ,(if description description "IS")))
          (result ,expected)
          (expected ,form)
          (verdict (if (eql result expected) :pass :failed)))
     (print-short-verdict
        (make-verdict :is source result expected verdict))))


(defmacro isnt (expected form &optional &key description skip)
  `(let* ((source (format t "~a.~a" test ,(if description description "IS")))
          (result ,expected)
          (expected ,form)
          (verdict (if (eql result expected) :pass :failed)))
     (print-short-verdict
        (make-verdict :is source result expected verdict))))

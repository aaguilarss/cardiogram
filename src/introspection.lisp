;;; This file is a part of cardiogram
;;; (c) 2019 Abraham Aguilar <a.aguilar@ciencias.unam.mx>


(uiop:define-package :cardiogram/introspection
  (:mix :closer-mop :cl)
  (:use :cardiogram/tests)
  (:export :show-deftest))
(in-package :cardiogram/introspection)


(defun show-deftest (test)
  (tagbody
    (typecase test
      (test (go :test)))
    :test
    (pprint
      `(deftest ,(test-options test)
                ,(test-body test)))))

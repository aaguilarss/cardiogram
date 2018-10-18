; Cardiogram
; Author: Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)

(asdf:defsystem :cardiogram
  :author "Abraham Aguilar"
  :maintainer "Abraham Aguilar"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://gitlab.com/a.aguilar/cardiogram"
  :depends-on (:alexandria
               :cl-annot
               :let-over-lambda)
  :components ((:module "src"
                :serial t
                :components
                  ((:file "toolkit")
                   (:file "fixtures")
                   (:file "valuations")
                   (:file "tests")
                   (:file "cardiogram"))))
  :description "Simple test framework for Common Lisp"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cardiogram-test))))

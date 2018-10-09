; Cardiogram
; Author: Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)

(asdf:defsystem :cardiogram-test
  :author "Abraham Aguilar <a.aguilar@ciencias.unam.mx>"
  :license "MIT"
  :description "Tests for cardiogram"
  :depends-on (:cardiogram)
  :components ((:module "t"
                :serial t
                :components
                ((:file "cardiogram")))))

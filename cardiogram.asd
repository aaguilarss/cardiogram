; Cardiogram
; Author: Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)

(asdf:defsystem :cardiogram
  :author "Abraham Aguilar"
  :maintainer "Abraham Aguilar"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/shapesncats/cardiogram"
  :bug-tracker "https://github.com/shapesncats/cardiogram/issues"
  :source-control (:git "git@github.com:shapesncats/cardiogram.git")
  :depends-on (:alexandria
               :trivial-types)
  :components ((:module "src"
                :serial t
                :components
                  ((:file "cardiogram"))))
  :description "Test framework"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cardiogram-test))))

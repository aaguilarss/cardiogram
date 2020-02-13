;; This file is part of cardiogram
;; (c) 2018 - Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(asdf:defsystem :cardiogram
  :author "Abraham Aguilar"
  :maintainer "Abraham Aguilar"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://gitlab.com/a.aguilar/cardiogram"
  :description "Simple test framework"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "cardiogram/test-system")))
  :class :package-inferred-system
  :pathname "src"
  :depends-on (:cardiogram/all))

(asdf:defsystem :cardiogram/test-system 
  :depends-on (:cardiogram)
  :pathname "tests"
  :components ((:file "fixtures"))
  :perform (test-op (o c) (uiop:symbol-call :cl 'print 4) ))

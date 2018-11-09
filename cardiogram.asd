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
  :class :package-inferred-system
  :pathname "src"
  :depends-on (:cardiogram/all))

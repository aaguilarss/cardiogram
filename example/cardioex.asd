;; This file is part of cardioex
;; (C) Abraham Aguilar abraham@fullandfaithful.com

(asdf:defsystem :cardioex
  :author "Abraham Aguilar"
  :maintainer "Abraham Aguilar"
  :licence "MIT"
  :version "0.1.0"
  :homepage "homepage"
  :description "description"
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :class :package-inferred-system
  :pathname "src"
  :depends-on (:cardioex/tests
                :cardioex/symbols))

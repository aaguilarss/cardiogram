#|
  This file is a part of cardiogram project.
  Copyright (c) 2018 Abraham Aguilar (a.aguilar@ciencias.unam.mx)
|#

#|
  Author: Abraham Aguilar (a.aguilar@ciencias.unam.mx)
|#

(defsystem "cardiogram"
  :version "0.1.0"
  :author "Abraham Aguilar"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "cardiogram"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "cardiogram-test"))))

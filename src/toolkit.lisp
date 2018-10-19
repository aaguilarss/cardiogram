;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)
(defpackage :cardio/toolkit
  (:use :cl))
(in-package :cardio/toolkit)
(annot:enable-annot-syntax)

@export
(defmacro delay (expr)
  `(lambda () ,expr))

@export
(defun force (thunk)
  (funcall thunk))

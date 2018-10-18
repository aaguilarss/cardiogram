;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)
(defpackage :cardiogram
  (:use :cl
        :cardio/fixtures
        :cardio/valuations
        :cardio/tests)
  (:nicknames
    :cardio)
  (:export :+fixes+
           :fix-bound-p
           :fix-definition
           :defix
           :with-fixtures
           :fix
           :f!block
           :f!let
           :f!let*)
  (:export :+formats+
           :+format+
           :find-format
           :deformat
           :deformat!
           :+valuations+
           :val-definition
           :call-valuation
           :defval
           :true
           :false
           :pass
           :fail
           :is
           :isnt
           :is-values
           :isnt-values
           :eql-types
           :expands-1)
  (:export :+tests+
           :deftest))


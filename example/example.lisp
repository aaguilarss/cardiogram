;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>
(in-package :cl-user)
(defpackage :cardio/example
  (:use :cl :cardiogram))
(in-package :cardio/example)


;;; Symbol factory

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun @s (&rest args)
  (values
    (intern (string-upcase (apply #'mkstr args)))))

(defun @k (&rest args)
  (values
    (intern (string-upcase (apply #'mkstr args))
            (find-package :keyword))))

(defun @g (&rest args)
  (gensym (string-upcase (apply #'mkstr args))))



;;; Tests


(deftest str ()
  (eql-types (mkstr 'h) "h")
  (is (@s)))

(deftest symb (:after (str))
  (eql-types (@s "hello") 'y)
  (is (@s "hello") 'hello))

(deftest key (:after (symb))
  (eql-types (@k "hello") :key)
  (is (@s "hello") :hello))

(deftest gen (:after (symb))
  (eql-types (@g "hello") 'y))

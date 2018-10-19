;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)
(defpackage :cardio/tests
  (:use :cl :lol
        :cardio/fixtures
        :cardio/toolkit))
(in-package :cardio/tests)
(annot:enable-annot-syntax)


@export
(defvar +tests+ (make-hash-table))


;;; test class

(defclass test ()
  ((forms
     :initarg :forms
     :accessor forms)
   (before
     :initarg :before
     :accessor before
     :initform nil)
   (after
     :initarg :after
     :accessor after
     :initform nil)
   (around
     :initarg :around
     :accessor around
     :initform nil)
   (time-limit
     :initarg :time-limit
     :accessor time-limit
     :initform nil)))

(defun find-test (name)
  (gethash name +tests+))

(defun (setf find-test) (new name)
  (setf (gethash name +tests+) new))

(defun test-definition (name)
  (forms (find-test name)))

(defun (setf test-definition) (new name)
  (setf (forms (find-test name)) new))


(defun resolve-test-combination (test before after around)
  "Resolve test combination for TEST"
  (maphash (lambda (k v) (when (member k before) (adjoinf test (before v))))
           +tests+)
  (maphash (lambda (k v) (when (member k after) (adjoinf test (after v))))
           +tests+)
  (maphash (lambda (k v) (when (member k around) (adjoinf test (around v))))
           +tests+))


@export
(defmacro deftest (name options &body body)
  "Creates an instance of test and saves it to +TESTS+.
  Then defines a dispatch function named NAME"
  `(progn
     (setf (find-test ',name)
           (make-instance 'test))
     (setf (test-definition ',name)
           (lambda ()
             ,@(alexandria:parse-body body :documentation t)))
     (resolve-test-combination ',name
                               ,(@l (getf options :before))
                               ,(@l (getf options :after))
                               ,(@l (getf options :around)))
     (setf (symbol-function ',name)
           (lol:dlambda
             (:run (&optional skip) (eval-test ',name skip))))))





@export
(defun eval-test (test skip)
  (let ((tst (find-test test)))
    (dolist (a (around tst))
      (unless (member a (@l skip))
        (eval-test a skip)))
    (dolist (a (before tst))
      (unless (member a (@l skip))
        (eval-test a skip)))
    (funcall (forms tst))
    (dolist (a (after tst))
      (unless (member a (@l skip))
        (eval-test a skip)))
    (dolist (a (around tst))
      (unless (member a (@l skip))
        (eval-test a skip)))))

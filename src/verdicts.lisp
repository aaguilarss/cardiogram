;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)
(defpackage :cardio/verdicts
  (:use :cl))
(in-package :cardio/verdicts)
(annot:enable-annot-syntax)


;;; Verdict classes

(defclass verdict ()
  ((source
     :initarg :source
     :accessor source)
   (result
     :initarg :result
     :accessor result)
   (expected
     :initarg :expected
     :accessor expected)
   (verdict
     :initarg :verdict
     :accessor verdict)))

(defclass true-verdict (verdict)
  (source verdict))

(defclass false-verdict (verdict)
  (source verdict))

(defclass pass-verdict (verdict)
  (source))

(defclass fail-verdict (verdict)
  (source))

(defclass is-verdict (verdict)
  (source result expected verdict))

(defclass isnt-verdict (verdict)
  (source result expected verdict))

(defclass is-values-verdict (verdict)
  (source result expected verdict))

(defclass isnt-values-verdict (verdict)
  (source result expected verdict))

(defclass of-type-verdict (verdict)
  (source result expected verdict))

(defclass is-error-verdict (verdict)
  (source result expected verdict))

(defclass is-expand-verdict (verdict)
  (source result expected verdict))

(defclass is-print-verdict (verdict)
  (source result expected verdict))

@export
(defun make-verdict (type &optional source result expected verdict)
  "Initialize instance of make-verdict"
  (make-instance (case type
                   (:fail 'fail-verdict)
                   (:pass 'pass-verdict)
                   (:true 'true-verdict)
                   (:false 'false-verdict)
                   (:is 'is-verdict)
                   (:isnt 'isnt-verdict)
                   (:of-type 'of-type-verdict)
                   (:is-error 'is-error-verdict)
                   (:is-expand 'is-expand-verdict)
                   (:is-print 'is-print-verdict)
                   (:is-values 'is-values-verdict)
                   (:isnt-values 'isnt-values-verdict))
                 :source source
                 :result result
                 :expected expected
                 :verdict verdict))

@export
(defgeneric print-short-verdict (verdict)
  (:documentation "Short description of VERDICT"))

(defmethod print-short-verdict ((v verdict))
  (format t "~a - ~a~%"
          (if (eql (verdict v) :pass) "P" "F")
          (source v)))


@export
(defgeneric print-detail-verdict (verdict)
  (:documentation "More detailed description of VERDICT"))

(defmethod print-detail-verdict ((v true-verdict))
  (format t "~a -~a-~% ~a was expected to be true~%~%"
          (source v)
          (if (eql (verdict v) :pass) "PASSED" "FAILED")
          (result v)))

(defmethod print-detail-verdict ((v false-verdict))
  (format t "~a -~a-~% ~a was expected to be false~%~%"
          (source v)
          (if (eql (verdict v) :pass) "PASSED" "FAILED")
          (result v)))

(defmethod print-detail-verdict ((v is-verdict))
  (format t "~a -~a-~% ~a was expected to be ~a~%~%"
          (source v)
          (if (eql (verdict v) :pass) "PASSED" "FAILED")
          (result v)
          (expected v)))

(defmethod print-detail-verdict ((v isnt-verdict))
  (format t "~a -~a-~% ~a was expected to be different from ~a~%~%"
          (source v)
          (if (eql (verdict v) :pass) "PASSED" "FAILED")
          (result v)
          (expected v)))

(defmethod print-detail-verdict ((v is-values-verdict))
  (format t "~a -~a-~% the values ~a were expected to be ~a~%~%"
          (source v)
          (if (eql (verdict v) :pass) "PASSED" "FAILED")
          (result v)
          (expected v)))

(defmethod print-detail-verdict ((v isnt-values-verdict))
  (format t "~a -~a-~% the values ~a were expected to be different from ~a~%~%"
          (source v)
          (if (eql (verdict v) :pass) "PASSED" "FAILED")
          (result v)
          (expected v)))

(defmethod print-detail-verdict ((v of-type-verdict))
  (format t "~a -~a-~% ~a was expected to be of type ~a~%~%"
          (source v)
          (if (eql (verdict v) :pass) "PASSED" "FAILED")
          (result v)
          (expected v)))

(defmethod print-detail-verdict ((v is-error-verdict))
  (format t "~a -~a-~% ~a was expected to be a ~a condition~%~%"
          (source v)
          (if (eql (verdict v) :pass) "PASSED" "FAILED")
          (result v)
          (expected v)))

(defmethod print-detail-verdict ((v is-expand-verdict))
  (format t "~a -~a-~% ~a was expected to expand to ~a~%~%"
          (source v)
          (if (eql (verdict v) :pass) "PASSED" "FAILED")
          (result v)
          (expected v)))

(defmethod print-detail-verdict ((v is-print-verdict))
  (format t "~a -~a-~% ~a was expected to be ~a~%~%"
          (source v)
          (if (eql (verdict v) :pass) "PASSED" "FAILED")
          (result v)
          (expected v)))

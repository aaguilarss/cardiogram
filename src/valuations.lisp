;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)
(defpackage :cardio/valuations
  (:use :cl))
(in-package :cardio/valuations)
(annot:enable-annot-syntax)


; Ultra basic functionality

(defparameter +formats+ (make-hash-table))
(defparameter +format+ nil)

(defun find-format (name)
  (gethash name +formats+))

(defun (setf find-format) (new name)
  (setf (gethash name +formats+) new))

(defmacro deformat (name args &body body)
  "Define a format."
  `(setf (find-format name)
         (lambda ,args
          ,@(alexandria:parse-body body :documentation t))))

(defmacro deformat! (name args &body body)
  "Define a format. This macro injects two variables: TEST and STREAM"
  `(setf (find-format name)
         (lambda ,(append '(test stream) args)
           ,@(alexandria:parse-body body :documentation t))))

(deformat! simple (expected form valuation-name)
  "The simple format. Used as DEFAULT"
  (format stream "~a - ~:[FAILED ~a was expected. Got ~a~%~;PASS~%~]"
          valuation-name test form expected))

(deformat! binary ()
  "More simple format. Prints 0 if failed, 1 if passed"
  (format stream "~:[0~;1~]~%" test))


(lambda (expected form &optional &key s)
  (funcall (find-format 'simple)
           (eql expected form) t expected form (valuation-name "S")))


;;; Valuations

(defparameter +valuations+ (make-hash-table))


(defun symbol-valuation (symbol)
  (gethash symbol +valuations+))

(defun (setf symbol-valuation) (new symbol)
  (setf (gethash symbol +valuations+) new))

(defmacro defvaluation (name args body)
  `(setf (symbol-valuation ,name)
         (lambda ,args
           ,@body)))


(defvaluation is (expected form)
  (eql expected form))


(defgeneric call-valuation (format valuation args &optional stream)
  (:documentation "Call VALUAION with FORMAT and ARGS.")
  (:argument-precedence-order format valuation args))



(call-valuation (make-instance 'is :expected this :form that)
                +format+)

(call-valuation (make-instance 'is :expected this :form that)
                +format+
                '(this that))

(defmethod call-valuation ((valuation cardio-valuation) (format binary) args)
  (let ((result (apply (valuator valuation) args)))
    (funcall (formatter format) result)))





(defval is (expected form))


(defmacro x? (&optional (x nil supplied-p))
  (if supplied-p
    x
    (print ":(")))

(defmacro tr (&optional (x nil x-p))
  (unless x-p
    (print ":("))
  x)




; Example of a is-not valuation

(lambda (expected form)
  (format t "IS-NOT - ~:[FAILED ~a was expected not to be ~a~%~;PASSED~%~]"
          (not (eql expected form))
          form
          expected))

(lambda (expected form)
  (format t "IS - ~:[FAILED ~a was expected to be ~a~%~;PASSED~%~]"
          (eql expected form)
          form
          expected))

(lambda (form)
  (format t "IS - ~:[FAILED ~a was expected to be ~a~%~;PASSED~%~]"
          (eql form t)
          form
          t))

(lambda (form)
  (format t "IS - ~:[FAILED ~a was expected to be ~a~%~;PASSED~%~]"
          (eql form nil)
          form
          nil))




(defmacro defval (name args msg test)
  `(defun ,name ,args
    (format t "~w - ~:[FAILED ~a was  not to be ~a~%~;PASSED~%~]"
            ',name ,test form expected)))

(lambda defval)



(defval is (expected form)
  (eql))

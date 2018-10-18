;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)
(defpackage :cardio/valuations
  (:use :cl))
(in-package :cardio/valuations)
(annot:enable-annot-syntax)


; Basic functionality
;;; Formats

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
  "Define a format. This macro injects two variables: TEST and STREAM as
  the first and second required arguments of the formatter funcion it
  defines."
  `(setf (find-format ',name)
         (lambda ,(append '(test stream) args)
           ,@(alexandria:parse-body body :documentation t))))


;; Built-in formats

(deformat! simple (form expected valuation-name)
  "The simple format. Used as DEFAULT"
  (format stream "~a - ~:[FAILED ~a was expected. Got ~a~%~;PASS~%~]"
          valuation-name test form expected))

(deformat! binary ()
  "More simple format. Prints 0 if failed, 1 if passed"
  (format stream "~:[0~;1~]~%" test))



;;; Valuations

(defparameter +valuations+ (make-hash-table))


(defun val-definition (symbol)
  (gethash symbol +valuations+))

(defun (setf val-definition) (new symbol)
  (setf (gethash symbol +valuations+) new))

(defgeneric call-valuation (format valuation args &optional stream)
  (:documentation "Defines how of FORMAT handles the result of VALUATION")
  (:argument-precedence-order format valuation args))


(defmacro defval (name args &body body)
  `(progn
     (setf (val-definition ',name)
           (lambda ,args
             ,@(alexandria:parse-body body :documentation t)))
     (defun ,name ,args
       (call-valuation +format+ ',name (list ,@args)))))



;; Built-in Valuations

(defval true (form)
  (when form t))

(defval false (form)
  (unless form t))

(defval pass (form)
  (declare (ignore form))
  t)

(defval fail (form)
  (declare (ignore form))
  nil)

(defval is (form expected)
  (eql expected form))

(defval isnt (form expected)
  (not (eql expected form)))

(defval is-values (form expected)
  (equal (multiple-value-list form)
         (multiple-value-list expected)))

(defval isnt-values (form expected)
  (not
    (equal (multiple-value-list form)
           (multiple-value-list expected))))

(defval eql-types (form expected)
  (eql (type-of form)
       (type-of expected)))

(defval expands-1 (form expected)
  (equal (macroexpand-1 form)
         expected))




;;; Format implementation

(defmethod call-valuation ((format (eql 'binary)) val args &optional (stream t))
  (funcall (find-format format) (apply (val-definition val) args) stream))

(defmethod call-valuation ((format (eql 'simple)) val args &optional (stream t))
  (funcall (find-format format)
           (apply (val-definition val) args)
           stream
           (car args)
           (cadr args)
           val))

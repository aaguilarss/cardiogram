;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)
(defpackage :cardio/valuations
  (:use :cl))
(in-package :cardio/valuations)
(annot:enable-annot-syntax)


; Basic functionality
;;; Formats

@export
(defparameter +formats+ (make-hash-table))

@export
(defparameter +format+ nil)

@export
(defun find-format (name)
  (gethash name +formats+))

@export
(defun (setf find-format) (new name)
  (setf (gethash name +formats+) new))

@export
(defmacro deformat (name args &body body)
  "Define a format."
  `(setf (find-format name)
         (lambda ,args
          ,@(alexandria:parse-body body :documentation t))))

@export
(defmacro deformat! (name args &body body)
  "Define a format. This macro injects two variables: RESULT and STREAM as
  the first and second required arguments of the formatter funcion it
  defines."
  `(setf (find-format ',name)
         (lambda ,(append '(result stream) args)
           ,@(alexandria:parse-body body :documentation t))))


;; Built-in formats

(deformat! simple (form expected valuation-name)
  "The simple format. Used as DEFAULT"
  (format stream "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
          result valuation-name expected form))

(deformat! binary ()
  "More simple format. Prints 0 if failed, 1 if passed"
  (format stream "~:[0~;1~]~%" test))

; Set simple format as default
(setf +format+ 'simple)


;;; Valuations

@export
(defparameter +valuations+ (make-hash-table))

@export
(defun val-definition (symbol)
  (gethash symbol +valuations+))

(defun (setf val-definition) (new symbol)
  (setf (gethash symbol +valuations+) new))

@export
(defgeneric call-valuation (format valuation args &optional stream)
  (:documentation "Defines how of FORMAT handles the result of VALUATION")
  (:argument-precedence-order format valuation args))

@export
(defmacro defval (name args &body body)
  `(progn
     (setf (val-definition ',name)
           (lambda ,args
             ,@(alexandria:parse-body body :documentation t)))
     (defun ,name ,args
       (call-valuation +format+ ',name (list ,@args)))))



;; Built-in Valuations

@export
(defval true (form)
  (when form t))

@export
(defval false (form)
  (unless form t))

@export
(defval pass (form)
  (declare (ignore form))
  t)

@export
(defval fail (form)
  (declare (ignore form))
  nil)

@export
(defval is (form expected)
  (eql expected form))

@export
(defval isnt (form expected)
  (not (eql expected form)))

@export
(defval is-values (form expected)
  (equal (multiple-value-list form)
         (multiple-value-list expected)))

@export
(defval isnt-values (form expected)
  (not
    (equal (multiple-value-list form)
           (multiple-value-list expected))))

@export
(defval eql-types (form expected)
  (eql (type-of form)
       (type-of expected)))

@export
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

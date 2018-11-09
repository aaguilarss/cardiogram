;; This file is part of cardiogram
;; (c) 2018 - Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/valuations
  (:mix :closer-mop :cl))
(in-package :cardiogram/valuations)


(defparameter +default-format+ nil)

(defclass valuation ()
  ((applicable-formats
     :initarg :applicable-formats
     :accessor valuation-applicable-formats
     :initform nil)
   (name
     :initarg :name
     :accessor valuation-name))
  (:metaclass funcallable-standard-class))

(defun vboundp (symbol)
  (and (fboundp symbol)
       (typep (symbol-function symbol) 'valuation)))


(defun find-valuation (valuation)
  (and (vboundp valuation)
       (symbol-function valuation)))

(defun (setf find-valuation) (new valuation)
  (setf (symbol-function valuation) new))

(defun add-format (valuation format-name format-lambda)
  (pushnew (cons format-name format-lambda)
           (valuation-applicable-formats (find-valuation valuation))))

(defun find-format-for-valuation (valuation format)
  (setf valuation (find-valuation valuation))
  (setf format (assoc format (valuation-applicable-formats valuation)))
  (if format
    (cdr format)
    (error "No applicable format found of name ~a" +default-format+)))

(defmethod initialize-instance :after ((val valuation) &key)
  (set-funcallable-instance-function val
    (lambda (&rest args)
      (apply
        (find-format-for-valuation
          (valuation-name val) +default-format+) args))))



;; Built-in valuations

(setf +default-format+ 'simple)

(defmacro define-simple-valuation (name args &body body)
  `(progn
     (setf (find-valuation ',name)
           (make-instance 'valuation :name ',name))
     (add-format ',name 'simple
                 (lambda ,args
                   ,@body))))

(define-simple-valuation pass (form)
  (declare (ignore form))
  (format t "PASSED - ~a ~%"
          'pass))

(define-simple-valuation fail (form)
  (declare (ignore form))
  (format t "FAILED - ~a ~%"
          'fail))

(define-simple-valuation true (form)
  (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
          form 'true t form))

(define-simple-valuation false (form)
  (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
          (not form) 'false nil form))

(define-simple-valuation is (form expected &key (test #'eql))
  (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
          (funcall test form expected) 'is expected form))

(define-simple-valuation isnt (form expected &key (test #'eql))
  (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
          (not (funcall test form expected)) 'isnt expected form))

(define-simple-valuation eql-types (form expected)
  (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
          (eql (type-of form) (type-of expected)) 'eql-types
          (type-of expected) (type-of form)))

(define-simple-valuation of-type (form expected)
  (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
          (typep form expected) 'of-type
          expected (type-of form)))

(define-simple-valuation expands-1 (form expected)
  (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
          (equal (macroexpand-1 form) expected) 'expands-1
          expected form))


;; Theese are redirected from macros.

(defmacro delay (&body form)
  `(lambda () ,@form))

(defun force (thunk)
  (funcall thunk))

(define-simple-valuation is-values% (form expected)
  (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
          (equal (multiple-value-list (force form))
                 (multiple-value-list (force expected)))
          'is-values
          (multiple-value-list (force expected))
          (multiple-value-list (force form))))

(defmacro is-values (form expected)
  `(funcall #'is-values% (delay ,form) (delay ,expected)))

(define-simple-valuation isnt-values% (form expected)
  (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
          (not (equal (multiple-value-list (force form))
                      (multiple-value-list (force expected))))
          'isnt-values
          (multiple-value-list (force expected))
          (multiple-value-list (force form))))

(defmacro isnt-values (form expected)
  `(funcall #'isnt-values% (delay ,form) (delay ,expected)))

(define-simple-valuation is-print% (form expected)
  (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
          (string= (with-output-to-string (*standard-output*)
                     (force form))
                   (force expected))
          'is-print
          (multiple-value-list (force expected))
          (multiple-value-list (force form))))

(defmacro is-print (form expected)
  `(funcall #'is-print% (delay ,form) (delay ,expected)))

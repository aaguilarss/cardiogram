;; This file is part of cardiogram
;; (c) 2018 - Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/valuations
  (:mix :closer-mop :cl)
  (:export :pass :fail :true :false :is
           :isnt :eql-types :of-type :expands-1
           :isnt-values :is-values :prints)
  (:export :vboundp :define-valuation :add-format
           :find-valuation :valuation-applicable-formats
           :valuation-name :valuation-test-forms))
(in-package :cardiogram/valuations)


(defparameter +default-format+ nil)

(defclass valuation ()
  ((applicable-formats
     :initarg :applicable-formats
     :accessor valuation-applicable-formats
     :initform nil)
   (name
     :initarg :name
     :accessor valuation-name)
   (test-forms
     :initarg :test-forms
     :accessor valuation-test-forms))
  (:metaclass funcallable-standard-class))

(defun vboundp (symbol)
  (and (fboundp symbol)
       (typep (symbol-function symbol) 'valuation)))


(defun find-valuation (valuation)
  (and (vboundp valuation)
       (symbol-function valuation)))

(defun (setf find-valuation) (new valuation)
  (setf (symbol-function valuation) new))


(defun find-format-for-valuation (valuation format)
  (setf valuation (find-valuation valuation))
  (setf format (assoc format (valuation-applicable-formats valuation)))
  (if format
    (cdr format)
    (error "No applicable format found of name ~a" +default-format+)))

(defun add-format (valuation format-name format-lambda)
  (pushnew (cons format-name format-lambda)
           (valuation-applicable-formats (find-valuation valuation))
           :test (lambda (x y) (eql (car x) (car y)))))

(defmethod initialize-instance :after ((val valuation) &key)
  (set-funcallable-instance-function val
    (lambda (&rest args)
      (let ((res (apply (valuation-test-forms val) args)))
        (funcall (find-format-for-valuation (valuation-name val) +default-format+)
                 args res)
        res))))

(defmacro define-valuation (name (&rest args) &body body)
  `(setf (find-valuation ',name)
         (make-instance 'valuation
                        :name ',name
                        :test-forms (lambda ,args ,@body))))

;; Built-in valuations

(defmacro delay (&body form)
  `(lambda () ,@form))

(defun force (thunk)
  (funcall thunk))



(define-valuation fail (form)
  (declare (ignore form)) nil)

(define-valuation pass (form)
  (declare (ignore form)) t)

(define-valuation true (form)
  (when form t))

(define-valuation false (form)
  (when (not form) t))

(define-valuation is (form expected &optional (test #'eql))
  (funcall test form expected))

(define-valuation isnt (form expected &optional (test #'eql))
  (not (funcall test form expected)))

(define-valuation eql-types (form expected)
  (eql (type-of form)
       (type-of expected)))

(define-valuation of-type (form expected-type)
  (typep form expected-type))

(define-valuation expands-1 (form expected)
  (equal (macroexpand-1 form) expected))


;; theese are redirected from macros

(define-valuation is-values% (form expected)
  (equal (multiple-value-list (force form))
         (multiple-value-list (force expected))))

(defmacro is-values (form expected)
  `(funcall #'is-values% (delay ,form) (delay ,expected)))


(define-valuation isnt-values% (form expected)
  (not (equal (multiple-value-list (force form))
              (multiple-value-list (force expected)))))

(defmacro isnt-values (form expected)
  `(funcall #'isnt-values% (delay ,form) (delay ,expected)))


(define-valuation prints% (form expecetd)
  (string= (with-output-to-string (*standard-output*)
             (force form)
             (force expected))))

(defmacro prints (form expected)
  `(funcall #'prints% (delay ,form) (delay ,expected)))



;; Format implementation

; Binary format

(loop for a in '(fail pass true false is isnt
                      is-values% isnt-values% prints%
                      expands-1 eql-types of-type)
      doing
      (add-format a 'binary
          (lambda (args res)
            (declare (ignore args))
            (format t "~:[F~%~;P~%~]") res)))


; Simple format

(setf +default-format+ 'simple)

(add-format 'fail 'simple
            (lambda (args res)
              (declare (ignore res))
              (destructuring-bind (form &rest rest)
                (format t "FAILED - ~a~%Got: ~a~%") 'fail form)))

(add-format 'pass 'simple
            (lambda (args res)
              (declare (ignore res))
              (destructuring-bind (form &rest rest)
                (format t "PASSED - ~a~%Got: ~a~%") 'pass form)))

(add-format 'true 'simple
  (lambda (args res)
    (destructuring-bind (form &rest rest)
      (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'true t form))))

(add-format 'false 'simple
  (lambda (args res)
    (destructuring-bind (form &rest rest)
      (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'true nil form))))

(add-format 'is 'simple
  (lambda (args res)
    (destructuring-bind (form expected &rest rest)
      (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'is expected form))))

(add-format 'isnt 'simple
  (lambda (args res)
    (destructuring-bind (form expected &rest rest)
      (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'isnt expected form))))

(add-format 'eql-types 'simple
  (lambda (args res)
    (destructuring-bind (form expected &rest rest)
      (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'eql-types (type-of expected) (type-of form)))))

(add-format 'eql-types 'simple
  (lambda (args res)
    (destructuring-bind (form expected &rest rest)
      (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'eql-types (type-of expected) (type-of form)))))

(add-format 'of-type 'simple
  (lambda (args res)
    (destructuring-bind (form expected &rest rest)
      (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'of-type expected (type-of form)))))

(add-format 'expands-1 'simple
  (lambda (args res)
    (destructuring-bind (form expected &rest rest)
      (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'expands-1 expected (macroexpand-1 form)))))

(add-format 'is-values% 'simple
  (lambda (args res)
    (destructuring-bind (form expected &rest rest)
      (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'is
              (mutliple-value-list (force expected))
              (multiple-value-list (expected form))))))

(add-format 'isnt-values% 'simple
  (lambda (args res)
    (destructuring-bind (form expected &rest rest)
      (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'is
              (mutliple-value-list (force expected))
              (multiple-value-list (expected form))))))

(add-format 'prints% 'simple
  (lambda (args res)
    (destructuring-bind (form expected &rest rest)
      (format t "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'is
              (with-output-to-string (*standard-output*) (force expected))
              (with-output-to-string (*standard-output*) (force form))))))

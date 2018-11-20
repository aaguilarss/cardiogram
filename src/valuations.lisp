;; This file is part of cardiogram
;; (c) 2018 - Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/valuations
  (:mix :closer-mop :cl)
  (:use :cardiogram/tests :cardiogram/toolkit)
  (:export :pass :fail :true :false :is
           :isnt :eql-types :of-type :expands-1
           :isnt-values :is-values :prints)
  (:export :vboundp :define-valuation :add-format
           :symbol-valuation :valuation-applicable-formats
           :valuation-name :valuation-test))
(in-package :cardiogram/valuations)


(defclass valuation ()
  ((applicable-formats
     :accessor valuation-applicable-formats
     :initform nil)
   (name
     :initarg :name
     :accessor valuation-name)
   (test
     :initarg :test
     :accessor valuation-test))
  (:metaclass funcallable-standard-class))

(defclass format-class ()
  ((formatter
     :initarg :formatter
     :accessor format-formatter
     :type (or string function))
   (name
     :initarg :name
     :accessor format-name))
  (:metaclass funcallable-standard-class))

(defmethod initialize-instance :after ((format format-class) &key)
  (set-funcallable-instance-function format
    (lambda (args result)
      (typecase (format-formatter format)
        (string
          (with-output-to-string (s)
            (apply #'format (l! s (format-formatter format) args result))))
        (function
          (funcall (format-formatter format) args result))))))


(defun vboundp (symbol)
  (and (fboundp symbol)
       (typep (symbol-function symbol) 'valuation)))

(defun symbol-valuation (valuation)
  (and (vboundp valuation)
       (symbol-function valuation)))

(defun (setf symbol-valuation) (new valuation)
  (cond
    ((vboundp valuation)
     (warn "Redefining valuation ~a in (SETF symbol-valuation)" valuation))
    ((fboundp valuation)
     (warn "Redefining ~a in (SETF symbol-valuation) previously fbound")))
  (setf (symbol-function valuation) new))

(defun add-format-to-valuation (valuation format)
  (setf (valuation-applicable-formats valuation)
        (remove-if (lambda (x) (eql (format-name x) (format-name format)))
                   (valuation-applicable-formats valuation)))
  (push format (valuation-applicable-formats valuation)))

(defun find-format-for-valuation (valuation)
  (or (find-if (lambda (x) (eql (format-name x) *default-format*))
               (valuation-applicable-formats valuation))
      (find-if (lambda (x) (eql (format-name x) 'binary))
               (valuation-applicable-formats valuation))))


(defmethod initialize-instance :after ((val valuation) &key)
  (add-format-to-valuation val
    (make-instance 'format-class
      :name 'binary
      :formatter (lambda (args res)
                   (declare (ignore args))
                   (if res "P" "F"))))
  (set-funcallable-instance-function val
    (lambda (&rest args)
      (declare (special *test*))
      (let ((result (apply (valuation-test val) args)))
        (push (cons result
                    (funcall (find-format-for-valuation val) args result))
              (test-results *test*))))))


(defmacro define-valuation (name (&rest args) &body body)
  `(setf (symbol-valuation ',name)
         (make-instance 'valuation
                        :name ',name
                        :test (lambda ,args ,@body))))

(defmacro define-format (valuation name args &body body)
  `(add-format-to-valuation (symbol-valuation ',valuation)
     (make-instance 'format-class :name ',name
       :formatter ,@(if args
                      `((lambda ,args ,@body))
                      body))))

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


(define-valuation time-limit% (form time-limit)
  (let (t1 t2)
    (setf t1 (get-internal-run-time))
    (force form)
    (setf t2 (get-internal-run-time))
    (< (/ (- t2 t1) internal-time-units-per-second) time-limit)))

(defmacro time-limit (form time-limit)
  `(funcall #'time-limit% (delay ,form) ,time-limit))


; Formatters


; Simple format

(define-format fail simple ()
  "FAILED - FAIL~%Got ~a~%")

(define-format pass simple ()
  "PASSED - PASS~%Got: ~a~%")

(define-format true simple (args res)
  (destructuring-bind (form &rest rest) args
    (with-output-to-string (s)
      (format s "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'true t form))))

(define-format false simple (args res)
  (destructuring-bind (form &rest rest) args
    (with-output-to-string (s)
     (format s "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
             res 'true nil form))))

(define-format is simple (args res)
  (destructuring-bind (form expected &rest rest) args
    (format nil "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
            res 'is expected form)))


(define-format isnt simple (args res)
  (destructuring-bind (form expected &rest rest) args
    (with-output-to-string (s)
      (format s "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'isnt expected form))))

(define-format eql-types simple (args res)
  (destructuring-bind (form expected &rest rest) args
    (with-output-to-string (s)
      (format s "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'eql-types (type-of expected) (type-of form)))))

(define-format eql-types simple (args res)
  (destructuring-bind (form expected &rest rest) args
    (with-output-to-string (s)
      (format s "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'eql-types (type-of expected) (type-of form)))))

(define-format of-type simple (args res)
  (destructuring-bind (form expected &rest rest) args
    (with-output-to-string (s)
      (format s "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
                  res 'of-type expected (type-of form)))))

(define-format expands-1 simple (args res)
  (destructuring-bind (form expected &rest rest) args
    (with-output-to-string (s)
      (format s "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'expands-1 expected (macroexpand-1 form)))))

(define-format is-values% simple (args res)
  (destructuring-bind (form expected &rest rest) args
    (with-output-to-string (s)
      (format s "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'is-values
              (mutliple-value-list (force expected))
              (multiple-value-list (expected form))))))

(define-format isnt-values% simple (args res)
  (destructuring-bind (form expected &rest rest) args
    (with-output-to-string (s)
      (format s "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'isnt-values
              (mutliple-value-list (force expected))
              (multiple-value-list (expected form))))))

(define-format prints% simple (args res)
  (destructuring-bind (form expected &rest rest) args
    (with-output-to-string (s)
      (format s "~:[FAILED - ~a ~%Expected: ~a ~%Got: ~a~% ~;PASSED - ~a~%~]"
              res 'is
            (with-output-to-string (*standard-output*) (force expected))
            (with-output-to-string (*standard-output*) (force form))
            (with-output-to-string (*standard-output*) (force expected))
            (with-output-to-string (*standard-output*) (force form))))))

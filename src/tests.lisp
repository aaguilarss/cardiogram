;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)
(defpackage :cardio-tests
  (:use :cl :lol))
(in-package :cardio-tests)

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
   (when
     :initarg :when
     :accessor test-when
     :initform t)
   (time-limit
     :initarg :time-limit
     :accessor time-limit
     :initform nil)
   (fixes
     :initarg :fixes
     :accessor fixes
     :initform nil)))


(defmacro deftest (name options &body body)
  "Creates an instance of test and saves it to +TESTS+.
  Then defines a dispatche function of name NAME"
  `(progn
     (setf (gethash ',name +tests+)
           (make-instance 'test
             :forms (lambda () ,@(alexandria:parse-body body :documentation t))
             :when ,(getf options :when)
             :time-limit ,(getf options :time-limit)))
     (maphash (lambda (k v)
                (when (member k ',(getf options :around))
                  (setf (around v)
                        (adjoin ',name (around v))))) +tests+)
     (maphash (lambda (k v)
                (when (member k ',(getf options :before))
                  (setf (before v)
                        (adjoin ',name (before v))))) +tests+)
     (maphash (lambda (k v)
                (when (member k ',(getf options :after))
                  (setf (after v)
                        (adjoin ',name (after v))))) +tests+)
    (setf (symbol-function ',name)
          (lol:dlambda
            (:run () (eval-test (gethash ',name +tests+)))
            (:info () "Info")))
    ',name))




(defun eval-test ((test test))
  (dolist (a (around test))
    (eval-test (gethash a +tests+)))
  (dolist (b (before test))
    (eval-test (gethash b +tests+)))
  (funcall (forms test))
  (dolist (a (after test))
    (eval-test (gethash a +tests+)))
  (dolist (a (around test))
    (eval-test (gethash a +tests+))))

; (opts
;   :before this ; Run the test before this
;   :after that ; Run the test after that
;   :arround this ; Run the test before and after this
;   :when (and this that) ; Run the test only if this condition is met
;   :time-limit 0.5 ; Run only for 0.5 seconds
;   :fix (v1 v2 v3) ; Fix these three variables
;   :on-exec nil) ; Run on execute


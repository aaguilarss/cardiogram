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
  `(setf (gethash ',name +tests+)
         (make-instance 'test
           :forms (lambda () ,@(alexandria:parse-body body :documentation t))
           :when ,(getf options :when)
           :before ,(getf options :before)
           :after ,(getf options :after)
           :around ,(getf options :around)
           :time-limit ,(getf options :time-limit)))
  `(alexandria:maphash-values
     (lambda (x)
       (loop for i in (reverse ,(getf options :around))
             do (setf (before x) (adjoin i (before x))))) +tests+)
  `(alexandria:maphash-values
     (lambda (x)
       (loop for i in (reverse ,(getf options :before))
             do (setf (before x) (adjoin i (before x))))) +tests+)
  `(alexandria:maphash-values
     (lambda (x)
       (loop for i in (reverse ,(getf options :after))
             do (setf (before x) (adjoin i (before x))))) +tests+)
  `(setf (symbol-function ',name)
         (dlambda
           (:run () "Running")
           (:info () "Info"))))



(defgenetic eval-test (test)
  (:documentation "Evaluate a test"))

(defmethod eval-test ((obj test))
  (funcall (forms test)))

(defmethod eval-test :around ((obj test))
  (dolist (tst (around obj)) (eval-test tst)))

(defmethod eval-test :before ((obj test))
  (dolist (tst (before obj)) (eval-test tst)))

(defmethod eval-test :after ((obj test))
  (dolist (tst (after obj))) (eval-test tst))


; (opts
;   :before this ; Run the test before this
;   :after that ; Run the test after that
;   :arround this ; Run the test before and after this
;   :when (and this that) ; Run the test only if this condition is met
;   :time-limit 0.5 ; Run only for 0.5 seconds
;   :fix (v1 v2 v3) ; Fix these three variables
;   :on-exec nil) ; Run on execute


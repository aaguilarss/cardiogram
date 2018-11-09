;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/tests
  (:use :cl :honey :cardiogram/fixtures :closer-mop)
  (:export :deftest :is :isnt))
(in-package :cardiogram/tests)


;; Test class

(defclass test-combination ()
  ((before
     :initarg :name
     :accessor test-combination-before)
   (after
     :initarg :after
     :accessor test-combination-after)
   (around
     :initarg :around
     :accessor test-combination-around)))

(defclass test ()
  ((forms
     :initarg :name
     :accessor test-forms)
   (combination
     :initarg :combination
     :accessor test-combination)
   (dependencies
     :initarg :dependencies
     :accessor test-dependencies)
   (result
     :initarg :result
     :accessor test-result)
   (time-limit
     :initarg :time-limit
     :accessor test-time-limit)
   (documentation
     :initarg :documentation
     :accessor test-documentation))
  (:metaclass funcallable-standard-class))


(defmethod initialize-intance :after ((test test) &key)
  (set-funcallable-instance-function test
    (construct-test-function test)))

(defun construct-test-function (test)
  (with-slots (forms combination dependencies) test
    (dlambda
      (:run (&rest options) (test-run-clause)))))

(defmacro test-run-clause ()
  `(let (around before after)
     (process-test-combination-given-options options)
     (funcall-tests-around-test)
     (funcall-tests-before-test)
     (run-this-test forms)
     (funcall-tests-after-test)
     (funcall-tests-around-test)
     (positive-resultp (test-result test))))

(defmacro process-test-combination-given-options (options)
  `(setq around
         (remove-if (lambda (x) (member x ,(getf options :skip)))
                    (test-combination-around combination))
         before
         (remove-if (lambda (x) (member x ,(getf options :skip)))
                    (test-combination-before combination))
         after
         (remove-if (lambda (x) (member x ,(getf options :skip)))
                    (test-combination-after combination))))


(defmacro funcall-tests-around-test ()
  `(dolist (a around)
     (funcall a)))

(defmacro funcall-tests-before-test ()
  `(dolist (a before)
     (funcall a)))

(defmacro funcall-tests-after-test ()
  `(dolist (a after)
     (funcall a)))

(defmacro! run-this-test (body)
  `(let (+results+ ,g!time1 ,g!time2)
     (if (dependency-assessment dependencies)
       (progn
         (push-passing-dependency-result)
         (setq ,g!time1 (get-internal-run-time))
         ,@body
         (setq ,g!time2 (get-internal-run-time))
         (push-test-time-result))
       (push-failing-dependency-result))
     (report-results)
     (set-test-result-using-results test +results+)))

(defun dependency-assessment (list)
  (loop for it in list collecting
        (typecase it
          (symbol it)
          (test `(= 2 ,it))
          (list (dependency-assessment it)))))

(defmacro push-test-time-result ()
  `(push t +results+))

(defmacro report-results ()
  `(dolist (r +results) (print r)))

(defun set-test-result-using-results (test +results+)
  `("hello"))

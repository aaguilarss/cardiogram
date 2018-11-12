;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/tests
  (:mix :closer-mop :cl)
  (:use :honey :cardiogram/valuations)
  (:export :deftest :*test-output* :tboundp
           :symbol-test :ensure-test))
(in-package :cardiogram/tests)


;; Test class

(defparameter *test-output* *standard-output*)

(defclass test ()
  ((forms
     :initarg :forms
     :accessor test-forms)
   (name
     :initarg :name
     :accessor test-foms)
   (before
     :initarg :before
     :accessor test-before)
   (after
     :initarg :after
     :accessor test-after)
   (around
     :initarg :around
     :accessor test-around)
   (dependencies
     :initarg :dependencies
     :accessor test-dependencies)
   (results
     :initarg :result
     :accessor test-results)
   (time-limit
     :initarg :time-limit
     :accessor test-time-limit)
   (documentation
     :initarg :documentation
     :accessor test-documentation))
  (:metaclass funcallable-standard-class))

(defun tboundp (symbol)
  (and (fboundp symbol)
       (typep (symbol-function symbol) 'test)))

(defun symbol-test (symbol)
  (and (tboundp symbol)
       (symbol-function symbol)))

(defun (setf symbol-test) (new symbol)
  (and (tboundp symbol)
       (setf (symbol-function symbol) new)))

(defun report-test (test)
  (dolist (res (reverse (test-results test)))
    (print (cdr res) *test-output*))
  (format *test-output* "Test ~a ~:[PASSED~;FAILED]~%~%"
          (test-name test) (every #'car (test-results test)))
  (values))

(defun add-result-to-test (test result)
  (push result (symbol-test (test-results test))))


(defun make-dependency-assessment (expr)
  (macrolet ((convert-dependency-expr (expr)
               (cons
                 (cond
                   ((member (car expr) '(:and and)) 'and)
                   ((member (car expr) '(:or or)) 'or)
                   (t 'and))
                 (loop for it in (cdr expr) collecting
                       (typecase it
                         (atom `(test-passedp ,it))
                         (list (convert-dependency-expr it)))))))
    (lambda () (convert-dependency-expr expr))))


(defun resolve-test-combination (test options)
  (loop for it in (getf options :before) doing
        (if (tboundp it)
          (push test (test-before (symbol-test it)))
          (error "No test named ~a found when resolving combination for ~a" it test)))
  (loop for it in (getf options :around) doing
        (if (tboundp it)
          (push test (test-around (symbol-test it)))
          (error "No test named ~a found when resolving combination for ~a" it test)))
  (loop for it in (getf options :after) doing
        (if (tboundp it)
          (push test (test-after (symbol-test it)))
          (error "No test named ~a found when resolving combination for test" it test))))

(defmethod initialize-intance :after ((test test) &key dependencies combination)
  (resolve-test-combination test combination)
  (setf (test-dependencies test)
        (make-dependency-assessment dependencies))
  (set-funcallable-instance-function test
    (lambda (&rest options)
      (with-slots (documentation name dependencies forms before time-limit) test
        (prog (time1 time2
                (skip (getf options :skip)))
          (unless (funcall dependencies)
            (go :out))
          (when (member :info options)
            (go :info))
          (format t "Running test ~a~%" (test-name test))
          (dolist (sy around)
            (or (member sy skip)
                (funcall (symbol-function sy))))
          (dolist (sy before)
            (or (member sy skip)
                (funcall (symbol-function sy))))
          (setf time1 (get-internal-run-time))
          (funcall forms)
          (setf time2 (get-internal-run-time))
          (dolist (sy after)
            (or (member sy skip)
                (funcall (symbol-function sy))))
          (dolist (sy around)
            (or (member sy skip)
                (funcall (symbol-function sy))))
          (let ((rntime (/ (- time2 time1)
                           internal-time-units-per-second)))
            (add-result-to-test name
              (cons (if time-limit (> time-limit rntime) t)
                    (format nil "Test took ~as to run." rntime))))
          :info
          (report-test test)
          (when (member :documentation options)
            (print documentation *test-output*))
          :out)))))

(defun ensure-test (name &rest initargs)
  (when (tboundp name)
    (warn "Redefining ~a previously defined as ~a."
          name (type-of (symbol-function name))))
  (setf (symbol-function name)
        (apply #'make-instance (cons test initargs))))

(defmacro transform-body (forms name)
  (loop for it in forms collecting
        (typecase it
          (list (if (vboundp (car it))
                  `(add-result-to-test ',name ,(transform-body it))
                  (transform-body it)))
          (atom it))))

(defmacro deftest (name (&rest options) &body body)
  (multiple-value-bind (bod decl doc) (parse-body body))
  `(ensure-test ',name
     :forms (lambda () ,@(transform-body bod))
     :name ',name
     :documentation ,doc
     :time-limit (getf options :time-limit)
     :dependencies (l! (getf options :depends-on))
     :combination ,(l!
                     :before (l! (getf options :before))
                     :around (l! (getf options :around))
                     :after (l! (getf options :after)))))

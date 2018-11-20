;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/tests
  (:mix :closer-mop :cl)
  (:use :honey)
  (:export :*test* :*test-output*)
  (:export :tboundp :symbol-test :ensure-test :deftest
           :test-results)
  (:export :deftest :*test-output* :tboundp :*test*
           :symbol-test :ensure-test)
  (:export :*default-format* :simple))
(in-package :cardiogram/tests)


;; Test class

(defparameter *test-output* *standard-output*)
(defparameter *default-format* 'simple)

(defclass test ()
  ((forms
     :initarg :forms
     :accessor test-forms)
   (name
     :initarg :name
     :accessor test-name)
   (before
     :initarg :before
     :initform nil
     :accessor test-before)
   (after
     :initarg :after
     :initform nil
     :accessor test-after)
   (around
     :initarg :around
     :initform nil
     :accessor test-around)
   (dependencies
     :initarg :dependencies
     :accessor test-dependencies)
   (results
     :initarg :results
     :initform nil
     :accessor test-results)
   (time-limit
     :initarg :time-limit
     :accessor test-time-limit)
   (status
     :initform nil
     :accessor test-status)
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
  (setf (symbol-function symbol) new))

(defun ensure-test-status (test)
  (unless (test-status test)
    (funcall test))
  (test-status test))

(defun test-passes-p (test)
  (case (ensure-test-status test)
    (:pass t)
    (:fail nil)))

(defun resolve-test-combination (test combination)
  (labels ((eqlnames (x y)
             (eql (test-name x) (test-name y))))
    (dolist (x (getf combination :before))
      (if (tboundp x)
        (pushnew test (test-before (symbol-test x))
                 :test #'eqlnames)))
    (dolist (x (getf combination :after))
      (if (tboundp x)
        (pushnew test (test-after (symbol-test x))
                :test #'eqlnames)))
    (dolist (x (getf combination :around))
      (if (tboundp x)
        (pushnew test (test-around (symbol-test x))
                :test #'eqlnames)))))

(defun check-dependencies (dependency-expr &aux test)
  (case (car dependency-expr)
    ((or :and nil) (setf test #'every))
    (:or (setf test #'some)))
  (funcall test
           (lambda (x)
             (typecase x
               (symbol (test-passes-p (symbol-test x)))
               (list (check-dependencies x))))
           (cdr dependency-expr)))

(defun compute-test-verdict-using-results (results)
  (every #'car results))




(defgeneric report-test (test format))

(defmethod report-test (test format)
  (format *test-output* "Test ~a ~a. ~%~%"
          (test-name test)
          (test-status test))
  (when (test-results test)
    (dolist (x (test-results test))
      (princ (cdr x) *test-output*)))
  (values))



(defmethod initialize-instance :after ((test test) &key)
  (set-funcallable-instance-function test
    (lambda (&rest options)
      (prog ((*test* test) (skip (getf options :skip)) t1 t2)
        (declare (special *test*))
        (unless (check-dependencies (test-dependencies test))
          (go :out))
        (when (member :info options)
          (go :info))
        (dolist (a (test-around test)) (unless (member a skip) (funcall a)))
        (dolist (a (test-before test)) (unless (member a skip) (funcall a)))
        (format *test-output* "Running test ~a...~%~%" (test-name test))
        :this
        (setf (test-results test) nil)
        (setf t1 (get-internal-run-time))
        (funcall (test-forms test))
        (setf t2 (get-internal-run-time))
        (format *test-output* "Test ~a took ~as to run~%~&" (test-name test)
                (float (/ (- t1 t2) internal-time-units-per-second)))
        (setf (test-status test)
              (if (compute-test-verdict-using-results (test-results test))
                :pass :fail))
        (report-test test *default-format*)
        :after
        (dolist (a (test-after test)) (unless (member a skip) (funcall a)))
        :around2
        (dolist (a (test-around test)) (unless (member a skip) (funcall a)))
        :info
        (when (or (member :info options)
                  (member :documentation options))
          (format *test-output* "Documentation:~% ~a ~%" (test-documentation test)))
        :out)
      (test-passes-p test))))


(defun resolve-dependency-of (test expr)
  (loop for sy in (cdr expr) do
        (when (tboundp sy)
          (setf (test-dependencies (symbol-test sy))
                `(,(car expr)
                   ,(test-name test)
                   ,(test-dependencies (symbol-test sy)))))))

(defun ensure-test (name initargs &key combination dependency-of)
  (when (tboundp name)
    (warn "Redefining ~a previously defined as ~a."
          name (type-of (symbol-function name))))
  (setf (symbol-function name)
        (apply #'make-instance (cons 'test initargs)))
  (resolve-test-combination (symbol-test name) combination)
  (resolve-dependency-of (symbol-test name) dependency-of)
  name)


(defun ensure-dependency-expr (expr)
  (case (car expr)
    (:and) (:or) (t (push :and expr)))
  (loop for el in expr collecting
        (typecase el
          (atom el)
          (list (ensure-dependency-expr el)))))


(defmacro deftest (name (&rest options) &body body)
  (multiple-value-bind (bod decl doc) (parse-body body)
    (declare (ignore decl))
    `(ensure-test ',name
       (list
         :forms (lambda () (declare (special *test*))
                  ,@bod)
         :name ',name
         :documentation ,doc
         :dependencies ',(ensure-dependency-expr (l! (getf options :depends-on)))
         :time-limit ,(getf options :time-limit))
       :combination '(
                      :around ,(l! (getf options :around))
                      :before ,(l! (getf options :before))
                      :after ,(l! (getf options :after)))
       :dependency-of ',(ensure-dependency-expr (l! (getf options :dependency-of))))))

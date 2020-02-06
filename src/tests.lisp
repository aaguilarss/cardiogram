;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/tests
  (:mix :closer-mop :cl)
  (:use :cardiogram/toolkit :cardiogram/conditions)
  (:export :*test* :*test-output*)
  (:export :tboundp :symbol-test :ensure-test :deftest
           :check-dependencies
           :test-results :test-dependencies :test-name
           :test-status :test-forms :test-after :test-before
           :test-around :test-passes-p :test-time-limit
           :test-forms :test-body :test-options)
  (:export :deftest :*test-output* :tboundp :*test* :*ignore-errors*
           :symbol-test :ensure-test :test)
  (:export :*default-format* :simple :binary))
(in-package :cardiogram/tests)


;; Test class

(defparameter *test-output* *standard-output*)
(defparameter *default-format* 'simple)

(defclass test ()
  ((forms
     :initarg :forms
     :accessor test-forms)
   (body
     :initarg :body
     :accessor test-body)
   (options
     :initarg :options
     :accessor test-options)
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
  (assert (tboundp symbol) () 'undefined-test :name symbol)
  (symbol-function symbol))

(defun (setf symbol-test) (new symbol)
  (setf (symbol-function symbol) new))

(defun ensure-test-status (test)
  (unless (test-status test)
    (funcall test))
  (test-status test))

(defun test-passes-p (test)
  (case (handler-case (ensure-test-status test)
          (test-failure (c) (declare (ignore c)) nil))
    (:passed t)
    (:failed nil)))

(defun resolve-test-combination (test combination)
  (handler-case
    (labels ((eqlnames (x y)
               (eql (test-name x) (test-name y))))
      (dolist (x (getf combination :before))
        (pushnew test (test-before (symbol-test x))
                   :test #'eqlnames))
      (dolist (x (getf combination :after))
        (pushnew test (test-after (symbol-test x))
                  :test #'eqlnames))
      (dolist (x (getf combination :around))
        (pushnew test (test-around (symbol-test x))
                  :test #'eqlnames)))
    (undefined-test (c)
      (error 'undefined-test-in-combination
             :name (undefined-test-name c)
             :combination-test (test-name test)))))

(defun check-dependencies (dependency-expr &aux test)
  (case (car dependency-expr)
    ((or :and nil) (setf test #'every))
    (:or (setf test #'some)))
  (funcall test
           (lambda (x)
             (handler-case
               (typecase x
                 (symbol (if (test-passes-p (symbol-test x)) t
                             (error 'test-dependencies-error :name x)))
                 (test (if (test-passes-p x) t
                           (error 'test-dependencies-error :name (test-name x))))
                 (list (check-dependencies x)))
               ((or undefined-test test-dependencies-error) (c)
                 (unless *ignore-test-errors*
                   (restart-case (invoke-debugger c)
                     (use-substitute (s)
                       :report "Use substitute test, symbol or dependency expr"
                       :interactive (lambda ()
                                      (format t "Please enter a substitute: ")
                                      (multiple-value-list (eval (read))))
                       (check-dependencies (list :and s)))
                     (continue ()
                       :report "Continue as if the test passed"
                       t))))))
           (cdr dependency-expr)))

;; This function needs to be customizable.
(defun compute-test-verdict-using-results (test)
  (if (every #'car (test-results test))
    t
    (unless *ignore-test-errors*
      (error 'test-failure :name (test-name test)))))


;; This function will also be customizable
(defgeneric report-test (test format))

(defmethod report-test (test format)
  (declare (ignorable format))
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
      (let ((*test* test) (skip (getf options :skip)) t1 t2)
        (declare (special *test*))
        (unless (check-dependencies (test-dependencies test))
          (push (cons nil (s! "Failed test dependencies"))
                (test-results test)))
        (dolist (a (test-around test)) (unless (member a skip) (funcall a)))
        (dolist (a (test-before test)) (unless (member a skip) (funcall a)))
        (format *test-output* "Running test ~a...~%" (test-name test))
        (setf (test-results test) nil)
        (handler-case (progn
                        (setf t1 (get-internal-real-time))
                        (funcall (test-forms test))
                        (setf t2 (get-internal-real-time)))
          (simple-condition (c)
            (push (cons nil (apply #'s!
                                   (l! (simple-condition-format-control c))
                                   (l! (simple-condition-format-arguments c))))
                  (test-results test))
            (unless *ignore-errors* (invoke-debugger c))))
        (let ((dt (float (/ (- t2 t1) internal-time-units-per-second))))
          (push (cons (if (test-time-limit test) (> (test-time-limit test) dt) t)
                      (s! "Test ~a took ~as to run ~%~&" (test-name test) dt))
                (test-results test)))
        (setf (test-status test)
              (if (compute-test-verdict-using-results test)
                :passed :failed))
        (report-test test *default-format*)
        (dolist (a (test-after test)) (unless (member a skip) (funcall a)))
        (dolist (a (test-around test)) (unless (member a skip) (funcall a)))
        (when (getf options :documentation)
          (push (cons t (s! "Documentation: ~% ~a ~%" (test-documentation test)))
                (test-results test))))
      (values (test-passes-p test)))))


(defun resolve-dependency-of (test expr)
  (handler-case
    (loop for sy in (cdr expr) do
          (setf (test-dependencies (symbol-test sy))
                `(,(car expr)
                   ,(test-name test)
                   ,(test-dependencies (symbol-test sy)))))
    (undefined-test (c)
      (error 'undefined-test-in-dependency-of
             :name (undefined-test-name c)
             :dependency-name (test-name test)))))

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
        (etypecase el
          (symbol (if (tboundp el)
                    el
                    (progn
                      (unless (eql el (or :and :or))
                        (warn "Test ~a is not currently defined." el))
                      el)))
          (test el)
          (list (ensure-dependency-expr el)))))



(defmacro deftest (name (&rest options) &body body)
  (multiple-value-bind (bod decl doc) (parse-body body)
    (declare (ignore decl))
    `(ensure-test ',name
       (list
         :options ',options
         :body ',body
         :forms (lambda () (declare (special *test*))
                  ,@bod)
         :name ',name
         :documentation ,doc
         :dependencies ',(ensure-dependency-expr (l! (getf options :depends-on)))
         :time-limit ,(getf options :time-limit))
       :combination '(:around ,(l! (getf options :around))
                      :before ,(l! (getf options :before))
                      :after ,(l! (getf options :after)))
       :dependency-of ',(ensure-dependency-expr (l! (getf options :dependency-of))))))



(defmacro test ((&rest options) &body body)
  (multiple-value-bind (bod decl doc) (parse-body body)
    (declare (ignore decl))
    `(let (instance)
       (setf instance
             (make-instance 'test
               :options ',options
               :body ',body
               :forms (lambda () (declare (special *test*))
                        ,@bod)
               :name 'anonymous-test
               :documentation ,doc
               :dependencies ',(ensure-dependency-expr (l! (getf options :depends-on)))
               :time-limit ,(getf options :time-limit)))
       (resolve-test-combination instance '(:around ,(l! (getf options :around))
                                            :before ,(l! (getf options :before))
                                            :after ,(l! (getf options :after))))
       (resolve-dependency-of instance
                              ',(ensure-dependency-expr (l! (getf options :dependency-of))))
       instance)))

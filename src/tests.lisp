;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)
(defpackage :cardio-tests
  (:use :cl))
(in-package :cardio-tests)

;;; Test class

(opts
  :before this ; Run the test before this
  :after that ; Run the test after that
  :arround this ; Run the test before and after this
  :when (and this that) ; Run the test only if this condition is met
  :unless (and this that) ; Run the test unless the condition is met
  :time-limit 0.5 ; Run only for 0.5 seconds
  :fixtures (v1 v2 v3) ; Fix these three variables
  :on-exec nil) ; Run on execute


(defclass test ()
  ((name :initarg :name :accessor)
   (forms
     :initarg :forms
     :accessor test-forms
     :type function) ; The actual test forms
   (before
     :initarg :before
     :accessor)
   (after :initarg :after :accessor)
   (arround :initarg :arround :accessor)
   (when :initarg :when :accessor) ; Default is t
   (unless :initarg :unless :accessor) ; Default is nil
   (time-limit :initarg :time-limit)
   (fixtures :initarg :fixtures)
   (on-exec :initarg :on-exec)))


(defmethod testcall ((test test))
  "Call test test"
  (dolist (e (before test)) (eval-test e)) ; Eval before tests
  (dolist (e (arround test)) (eval-test e)) ; Eval arround tests
  (when (and (twhen test)
             (not (tunless test)))
    (funcall (test-forms test)))
  (dolist (e (arround test)) (eval-test e)) ; Eval arround tests
  (dolist (e (after test)) (eval-test e))) ; Eval after tests




(lambda ()
  form
  :skip
  form)



(let ((skip '(a)))
  form
  form
  :a form
  form)

(tagbody
  :tag
  :tag
  :tag
  :tag
  :a
  :tag)


(only)

(tagbody
  :1
  :2
  :3
  :a
  :5
  :6)

(defun pop)


(let ((skip '(2)))
  (tagbody
    :eval
    (if skip
      (go 1)
      (go :end))
    1 (print "hello")
    (go :eval)
    2 (print " bye")
    (go :eval)
    3 (print " friend")
    (go :eval)
    :end))


(defmacro sklambda (name &body body)
  "lambda form that skips when it's name is passed as an argument"
  (alexandria:with-gensyms (x)
    `(lambda (&optional ,x)
       (unless (if (listp ,x)
                 (member ,name ,x)
                 (eql ,name ,x))
         ,@body))))


(it this that :a)

(lambda (x)
  (unless (if (listp x)
            (member :name x)
            (eql :name x))
    (do this)))

(lambda (x)
  (unless (eql x :skip)
    forms))

(sklambda :skip
          (print "hello"))

(if (x)
  (nil)
  (block nil
         forms))


(tagbody
  form)

(prog ((a 3))
      1 (princ "hello") (go a)
      2 (princ "hey")
      3 (princ ", lisa"))


:skip
(is :skip)

(defmethod eval-test ((test test) &key skip)
  (tagbody
    :before
    :after
    :arround
    :test
    ()))

(defmethod eval-test ((test test) &key skip))

(tagbody
  :
  :assertion)

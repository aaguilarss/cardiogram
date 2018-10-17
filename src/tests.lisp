;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)
(defpackage :cardio/tests
  (:use :cl :lol))
(in-package :cardio/tests)
(annot:enable-annot-syntax)

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
  ((forms
     :initarg :forms
     :accessor test-forms
     :type function) ; The actual test forms
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
   (time-limit
     :initarg :time-limit
     :accessor time-limit
     :initform nil)))

@export
(defmacro deftest (name options &body body)
  "Creates an instance of test and saves it to +TESTS+.
  Then defines a dispatche function of name NAME"
  `(progn
     (setf (gethash ',name +tests+)
           (make-instance 'test
             :forms (lambda () (with-fixtures ,(getf options :fix)
                                              (when ,(or (getf options :when) t)
                                                (f!block nil
                                                  ,@(alexandria:parse-body body :documentation t)))))
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
            (:run (&optional &key skip) (eval-test ',name skip))
            (:info () "Info")))
    ',name))


(defun eval-test (test skip)
  (let ((tst (gethash test +tests+)))
    (dolist (a (around tst))
      (unless (if (listp skip) (member a skip) (eql a skip))
        (eval-test a skip)))
    (dolist (a (before tst))
      (unless (if (listp skip) (member a skip) (eql a skip))
        (eval-test a skip)))
    (funcall (forms tst))
    (dolist (a (after tst))
      (unless (if (listp skip) (member a skip) (eql a skip))
        (eval-test a skip)))
    (dolist (a (reverse (around tst)))
      (unless (if (listp skip) (member a skip) (eql a skip))
        (eval-test a skip)))))

; (opts
;   :before this ; Run the test before this
;   :after that ; Run the test after that
;   :arround this ; Run the test before and after this
;   :when (and this that) ; Run the test only if this condition is met
;   :time-limit 0.5 ; Run only for 0.5 seconds
;   :fix (v1 v2 v3) ; Fix these three variables
;   :on-exec nil) ; Run on execute

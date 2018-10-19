;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)
(defpackage :cardio/toolkit
  (:use :cl))
(in-package :cardio/toolkit)
(annot:enable-annot-syntax)

@export
(defmacro delay (&body expr)
  `(lambda () ,@expr))

@export
(defun force (thunk)
  (funcall thunk))

@export
(defun adjoinf (new place)
  (setf place (adjoin new place)))


@export
(defun @l (&rest args)
  (loop while args
        for a in args appending
        (if (listp a)
          (loop for e in (pop args)
                collecting
                (if (eql e '~a)
                  (pop args)
                  e))
          `(,(pop args)))))

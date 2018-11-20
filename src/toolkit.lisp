;; This file is part of cardiogram
;; (c) 2018 - Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/toolkit
  (:use :cl)
  (:export :parse-body :l!))
(in-package :cardiogram/toolkit)

(defun parse-body (body-expr &optional &key (strict t))
  "Returns (values body declarations docstring)
  both body and declarations are returned as lists, docstring as a string"
  (let (docstring declarations forms)
    (tagbody
      (if strict
        (go :strict)
        (go :weak))
      :strict
      (dolist (a body-expr)
        (typecase a
          (string
            (if docstring
              (push a forms)
              (setf docstring a)))
          (list
            (if (and (eql (car a) 'declare) (null forms))
              (push a declarations)
              (push a forms)))
          (otherwise
            (push a forms))))
      (go :out)
      :weak
      (dolist (a body-expr)
        (typecase a
          (string
            (if docstring
              (push a forms)
              (setf docstring a)))
          (list
            (if (eql (car a) 'declare)
              (push a declarations)
              (push a forms)))
          (otherwise
            (push a forms))))
      (go :out)
      :out)
    (if (and (null forms) docstring)
      (values (list docstring) (nreverse declarations) nil)
      (values (nreverse forms) (nreverse declarations) docstring))))

(defun l! (&rest args)
  (loop while args appending
        (let ((it (pop args)))
          (typecase it
            (list
              (loop for e in it collecting
                    (if (eql e '~a) (pop args) e)))
            (function
              (mapcar it (pop args)))
            (otherwise `(,it))))))

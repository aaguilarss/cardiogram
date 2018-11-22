;; This file is part of cardiogram
;; (c) 2018 - Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/toolkit
  (:use :cl)
  (:export :parse-body :l! :s! :sy! :flatten))
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

(defun flatten (list)
  (labels ((recurse (x acc)
             (typecase x
               (null x acc)
               #+sbcl
               (sb-impl::comma (recurse (sb-impl::comma-expr x) acc))
               (atom (cons x acc))
               (otherwise (recurse (car x) (recurse (cdr x) acc))))))
    (recurse list nil)))

(defun s! (&rest args)
  "Build string PRINCing args. If at any point in the args
  a string with the char ~ is found, this string is treated
  as a FORMAT control string taking as many arguments from args
  as ~'s are found in the control string"
  (with-output-to-string (s)
    (loop while args doing
          (let ((it (pop args)))
            (typecase it
              (string
                (if (member #\~ (coerce it 'list))
                  (let ((n (count-if
                             (lambda (x) (equal #\~ x))
                             (coerce it 'list))))
                    (apply #'format (l! s it (loop for i from 1 to n collecting
                                                   (pop args)))))
                  (princ it s)))
              (otherwise (princ it s)))))))

(defun sy! (&rest args)
  (values (string-upcase (apply #'s! args))))

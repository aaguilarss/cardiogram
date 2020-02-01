;; This file is part of cardiogram
;; (c) 2018 - Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/annotations
  (:use :cl :cl-annot)
  (:export :in))

(in-package :cardiogram/annotations)

(labels ((e!symbolp (s)
           (and (symbolp s)
                (< 2 (length (symbol-name s)))
                (string= (symbol-name s) "E!"
                         :end1 1
                         :end2 1))))
  (defannotation in (x def)
    (:arity 2 :inline t)
    (let* ((pname
             (if (e!symbolp x)
               (intern (subseq (symbol-name x) 2) :keyword) x))
           (fname
             (intern (symbol-name (cadr def)) pname)))
       `(progn
          ,(when (e!symbolp x)
             `(export ',fname ,pname))
          ,(subst fname (cadr def) def)))))

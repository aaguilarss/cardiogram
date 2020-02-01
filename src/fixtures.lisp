;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/fixtures
  (:use :cl :cardiogram/toolkit)
  (:export :defix :with-fixtures
           :f!let :f!labels :f!let* :f!block))
(in-package :cardiogram/fixtures)



(defparameter *fixes* nil) 

(defmacro defix (args &body body)
  `(pushnew (lambda ,args ,@body) *fixes*))



;;; Built-in fixes

;; Fixes are written to expand intro macros, see how make-fixes works
;;  to know why. There's no need to name them tho.

(defix (s)
  (when (and (boundp s)
             (not #+:lispworks (sys:symbol-constant-p s)
                  #-:lispworks (constantp s)))
    `(setf (symbol-value ',s) ,(symbol-value s))))

(defix (s)
  (when (and (macro-function s)
             (not #+sbcl (sb-ext:package-locked-p (symbol-package s))
                  #-sbcl (eql (find-package :cl) (symbol-package s))))
    `(setf (macro-function ',s) ,(macro-function s))))

(defix (s)
  (when (and (fboundp s)
             (not (macro-function s))
             (not #+sbcl (sb-ext:package-locked-p (symbol-package s))
                  #-sbcl (eql (find-package :cl) (symbol-package s))))
    `(setf (fdefinition ',s) ,(fdefinition s))))



(defun make-fixes (symbol-list)
  (remove-if #'null
    (loop for s in symbol-list appending
          (when (and (not (keywordp s))
                     (symbol-package s))
            (loop for f in *fixes*
                  collecting (funcall f s))))))
              

(defmacro with-fixtures (symbols &body body)
  "Run the forms in BODY and fix the SYMBOLS"
  `(unwind-protect
     (progn ,@body)
     ,@(make-fixes symbols)))


;;; Blocks with automatic fixtures

(labels ((f!symbol-p (symbol)
                     (and
                       (symbolp symbol)
                       (< 2 (length (symbol-name symbol)))
                       (string= (symbol-name symbol)
                                "F!" :end1 2)))
         (f!symbol->symbol (symbol)
                           (and
                             (f!symbol-p symbol)
                             (sy! (subseq (symbol-name symbol) 2)))))


  (defmacro f!block (name &body body)
    "Block that fixes the"
    (let* ((fs (remove-if-not #'f!symbol-p (remove-duplicates (flatten body))))
           (ns (mapcar #'f!symbol->symbol fs)))
      `(unwind-protect
         (block ,name
                ,@(loop with bod = body
                        for a in ns
                        for b in fs
                        do (setf bod (subst a b bod))
                        return bod))
         ,@(make-fixes ns))))

  (defmacro f!let (bindings &body body)
    "Block that fixes the"
    (let* ((fs (remove-if-not #'f!symbol-p (remove-duplicates (flatten body))))
           (ns (mapcar #'f!symbol->symbol fs)))
      `(unwind-protect
         (let ,bindings
           ,@(loop with bod = body
                   for a in ns
                   for b in fs
                   do (setf bod (subst a b bod))
                   return bod))
         ,@(make-fixes ns))))

  (defmacro f!let* (bindings &body body)
    "Block that fixes the"
    (let* ((fs (remove-if-not #'f!symbol-p (remove-duplicates (flatten body))))
           (ns (mapcar #'f!symbol->symbol fs)))
      `(unwind-protect
         (let* ,bindings
           ,@(loop with bod = body
                   for a in ns
                   for b in fs
                   do (setf bod (subst a b bod))
                   return bod))
         ,@(make-fixes ns))))

  (defmacro f!labels (bindings &body body)
    "Block that fixes the"
    (let* ((fs (remove-if-not #'f!symbol-p (remove-duplicates (flatten body))))
           (ns (mapcar #'f!symbol->symbol fs)))
      `(unwind-protect
         (labels ,bindings
           ,@(loop with bod = body
                   for a in ns
                   for b in fs
                   do (setf bod (subst a b bod))
                   return bod))
         ,@(make-fixes ns)))))

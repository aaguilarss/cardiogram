;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/fixtures
  (:use :cl :honey)
  (:export :defix :with-fixtures)
  (:export :f!let :f!labels :f!let* :f!block))
(in-package :cardiogram/fixtures)



(defparameter *fixes* (make-hash-table))

(defun fix-bound-p (symbol)
  (when (gethash symbol *fixes*) t))

(defun fix-definition (symbol)
  (and (fix-bound-p symbol)
       (gethash symbol *fixes*)))

(defun (setf fix-definition) (new symbol)
  (etypecase new
    (function (setf (gethash symbol *fixes*) new))))

(defmacro defix (name args &body body)
  `(setf (fix-definition ',name)
         (lambda ,args
           ,@body)))



;;; Built-in fixes

(defix var (s)
  (when (and (boundp s)
             (not #+:lispworks (sys:symbol-constant-p s)
                  #-:lispworks (constantp s)))
    `(setf (symbol-value ',s) ,(symbol-value s))))

(defix macro-function (s)
  (when (and (macro-function s)
             (not #+sbcl (sb-ext:package-locked-p (symbol-package s))
                  #-sbcl (eql (find-package :cl) (symbol-package s))))
    `(setf (macro-function ',s) ,(macro-function s))))

(defix fdefinition (s)
  (when (and (fboundp s)
             (not (macro-function s))
             (not #+sbcl (sb-ext:package-locked-p (symbol-package s))
                  #-sbcl (eql (find-package :cl) (symbol-package s))))
    `(setf (fdefinition ',s) ,(fdefinition s))))



(defun make-fixes (symbol-list)
  (remove-if #'null
    (loop for s in symbol-list appending
          (typecase s
              (symbol
                (when (and (not (keywordp s))
                           (symbol-package s))
                  (loop for f being each hash-value of *fixes*
                        collecting (funcall f s))))
              (list
                (when (and (not (keywordp (car s)))
                           (symbol-package (car s)))
                  (loop for f in (cdr s)
                        collecting (funcall (fix-definition f) (car s)))))))))

(defmacro with-fixtures (symbols &body body)
  "Run the forms in BODY and fix the SYMBOLS"
  `(unwind-protect
     (block nil ,@body)
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

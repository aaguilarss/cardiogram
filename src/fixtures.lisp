;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(uiop:define-package :cardiogram/fixtures
  (:nicknames :cardiofix)
  (:mix :closer-mop :cl :cardiogram/toolkit)
  (:export :defix :with-fixtures
           :f!let :f!labels :f!let* :f!block))
(in-package :cardiogram/fixtures)



(defparameter *fixes* (make-hash-table))

(defclass fix ()
  ((auto
     :initarg :auto
     :accessor fix-autop))
  (:metaclass funcallable-standard-class))

(defun symbol-fix (symbol)
  (and (symbolp symbol)
       (gethash (k! symbol) *fixes*)))

(defun (setf symbol-fix) (new symbol)
  (etypecase new
    (function (setf (gethash (k! symbol) *fixes*) new))))

(defmacro defix (name (s &optional &key (auto t)) &body body)
  `(let ((insta))
     (setf insta
           (make-instance 'fix :auto ,auto))
     (set-funcallable-instance-function insta
       (lambda (,s)
          ,@body))
     (setf (symbol-fix ',name) insta)))



;;; Built-in fixes

(defix value (s)
  (when (and (boundp s)
             (not #+:lispworks (sys:symbol-constant-p s)
                  #-:lispworks (constantp s)))
    `(setf (symbol-value ',s) ,(symbol-value s))))

(defix macro (s)
  (when (and (macro-function s)
             (not #+sbcl (sb-ext:package-locked-p (symbol-package s))
                  #-sbcl (eql (find-package :cl) (symbol-package s))))
    `(setf (macro-function ',s) ,(macro-function s))))

(defix function (s)
  (when (and (fboundp s)
             (not (macro-function s))
             (not #+sbcl (sb-ext:package-locked-p (symbol-package s))
                  #-sbcl (eql (find-package :cl) (symbol-package s))))
    `(setf (fdefinition ',s) ,(fdefinition s))))


(defun specified-fixes (spec)
  (case (car spec)
    (:only
      (loop for s in (cdr spec) collecting
            (symbol-fix s)))
    (:all
      (loop for f being each hash-value of *fixes* collecting f))
    (:all-but
      (loop for f being each hash-key of *fixes* collecting
            (unless (member f spec)
              (symbol-fix f)) into fx
            finally
            (return (remove-if #'null fx))))
    (:and
      (loop for f being each hash-value of *fixes* collecting
            (when (fix-autop f) f) into fx
            finally
            (return (remove-if #'null
                               (remove-duplicates
                                 (append fx
                                   (loop for s in (cdr spec)
                                         collecting
                                         (symbol-fix s)))))))))) 
            
(defun make-fixes (symbol-list)
  (loop for s in symbol-list appending
        (typecase s
          (symbol
            (when (and (not (keywordp s))
                       (symbol-package s))
              (loop for f being each hash-value of *fixes*
                    collecting
                    (when (fix-autop f)
                      (funcall f s))
                    into exps
                    finally
                    (return (remove-if #'null exps)))))
          (list
            (when (and (not (keywordp (car s)))
                       (symbol-package (car s)))
              (loop for f in (specified-fixes (cdr s))
                    collecting
                    (funcall f (car s))))))
        into fixes
        finally (return (remove-if #'null fixes))))



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

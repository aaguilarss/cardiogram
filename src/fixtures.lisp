;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)
(defpackage :cardio/fixtures)
(in-package :cardio/fixtures)
(annot:enable-annot-syntax)


(defun make-fixes (symbol-list)
  (labels ((make-fix (s)
             (list
               (when (and (boundp s)
                          (not #+:lispworks (sys:symbol-constant-p s)
                               #-:lispworks (constantp s)))
                 `(setf (symbol-value ',s) ,(symbol-value s)))
               (when (and (macro-function s)
                          (not #+sbcl (sb-ext:package-locked-p (symbol-package s))
                               #-sbcl (eql (find-package :cl) (symbol-package s))))
                 `(setf (macro-function ',s) ,(macro-function s)))
               (when (and (fboundp s)
                          (not (macro-function s))
                          (not #+sbcl (sb-ext:package-locked-p (symbol-package s))
                               #-sbcl (eql (find-package :cl) (symbol-package s))))
                 `(setf (fdefinition ',s) ,(fdefinition s))))))

      (remove-if #'null (mapcan
                          (lambda (s)
                            (when (and (not (keywordp s))
                                       (symbol-package s))
                              (make-fix s)))
                          symbol-list))))




@export
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
                             (alexandria:symbolicate (subseq (symbol-name symbol) 2)))))


  @export
  (defmacro f!block (name &body body)
    "Block that fixes the"
    (let* ((fs (remove-if-not #'f!symbol-p (remove-duplicates (alexandria:flatten body))))
           (ns (mapcar #'f!symbol->symbol fs)))
      `(unwind-protect
         (block ,name
                ,@(loop with bod = body
                        for a in ns
                        for b in fs
                        do (setf bod (subst a b bod))
                        return bod))
         ,@(make-fixes ns))))

  @export
  (defmacro f!let (bindings &body body)
    "Block that fixes the"
    (let* ((fs (remove-if-not #'f!symbol-p (remove-duplicates (alexandria:flatten body))))
           (ns (mapcar #'f!symbol->symbol fs)))
      `(unwind-protect
         (let ,bindings
           ,@(loop with bod = body
                   for a in ns
                   for b in fs
                   do (setf bod (subst a b bod))
                   return bod))
         ,@(make-fixes ns))))

  @export
  (defmacro f!let* (bindings &body body)
    "Block that fixes the"
    (let* ((fs (remove-if-not #'f!symbol-p (remove-duplicates (alexandria:flatten body))))
           (ns (mapcar #'f!symbol->symbol fs)))
      `(unwind-protect
         (let* ,bindings
           ,@(loop with bod = body
                   for a in ns
                   for b in fs
                   do (setf bod (subst a b bod))
                   return bod))
         ,@(make-fixes ns)))))

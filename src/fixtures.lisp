;;; This file is a part of cardiogram
;;; (c) 2018 Abraham Aguilar <a.aguilar@ciencias.unam.mx>

(in-package :cl-user)
(defpackage :cardio/fixtures)
(in-package :cardio/fixtures)
(annot:enable-annot-syntax)

(macrolet ((make-fix (symbol)
             (let ((s (and (boundp symbol)
                           (not #+:lispworks (sys:symbol-constant-p symbol)
                                #-:lispworks (constantp symbol))))
                   (m (and (macro-function symbol)
                           (not #+sbcl (sb-ext:package-locked-p symbol)
                                #-sbcl (eql (find-package :cl) symbol))))
                   (f (and (fdefinition symbol)
                           (not (macro-function symbol))
                           (not #+sbcl (sb-ext:package-locked-p symbol)
                                #-sbcl (eql (find-package :cl) symbol)))))
               (when (or s m f)
                 `(,(when s `(setf (symbol-value ,symbol) ,(symbol-value symbol)))
                   ,(when m `(setf (macro-function ,symbol) ,(macro-function symbol)))
                   ,(when f `(setf (fdefinition ,symbol) ,(fdefinition symbol))))))))

  (defun make-fixes (symbol-list)
    "Returns a list of SETF forms.
    Each lambda form fixes, when applicable, the SYMBOL-VALUE,
    MACRO-FUNCTION, and FDEFINITION properties of each SYMBOL in SYMBOL-LIST"
    (alexandria:flatten
      (dolist (s symbol-list)
        (etypecase s
          (symbol
            (when (and (not (keywordp s))
                     (symbol-package s))
              (make-fix s)))
          (package
            (loop for x being the symbols of s
                  collect (make-fix s))))))))



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

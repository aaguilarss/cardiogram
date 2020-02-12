;; This file is part of cardioex
;; (C) Abraham Aguilar abraham@fullandfaithful.com

(uiop:define-package :cardioex/symbols
  (:use :cl :cardiogram)) 
(in-package :cardioex/symbols)

(annot:enable-annot-syntax)

@export
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

@in :cxt
(deftest l! ()
  (is (l! 1 '(2) '(3 4)) 
    '(1 2 3 4) #'equal)
  (is (l! #'1+ '(1 2 3 4))
    '(2 3 4 5) #'equal)
  (is (l! '(1 2 ~a 4 5) 3 6)
    '(1 2 3 4 5 6) #'equal))


@export
(defun s! (&rest args)
  (with-output-to-string (s)
    (loop while args doing
      (let ((it (pop args)))
        (typecase it
          (string
            (if (member #\~ (coerce it 'list))
              (let ((n (count-if
                         (lambda (x) (equal #\~ x))
                         (coerce it 'list))))
                (apply #'format (l! s it
                                    (loop for i from 1 to n
                                          collecting (pop args)))))
              (princ it s)))
          (otherwise (princ it s)))))))

@in :cxt
(deftest s! (:after cxt::l!)
  (is (s! "string1" "string2") "string1string2" #'string=)
  (is (s! "hello ~a" "world") "hello world" #'string=)
  (is (s! "hello " 'world) "hello WORLD" #'string=))


@export
(defun sy! (&rest args)
  (values (intern (string-upcase (apply #'s! args)))))

@in :cxt
(deftest sy! (:after cxt::s!)
  (of-type (sy! :hello)'symbol)
  (is (symbol-name (sy! "Hi"))
      (symbol-name 'hi) #'string=)
  (is (symbol-name (sy! "H~a" "ello"))
      (symbol-name 'hello) #'string=)) 

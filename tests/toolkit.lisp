;; This file is part of cardiogram
;; (c) 2020 - Abraham Aguilar <a.aguilar@ciencias.unam.mx>

;; Tests for the Toolkit   

(in-package :cardiotest)

; parse-body
(multiple-value-bind (body decl docs)
  (parse-body '("Docstring"
                (declare (ignorable x))
                (declare (ignore y))
                "Not Docstring"
                form1 form2 form3))
  (assert (equal body '("Not Docstring" form1 form2)))
  (assert (equal decl '((declare (ignorable x))
                        (declare (ignore y)))))
  (assert (string= docs "Docstring")))

(multiple-value-bind (body decl docs)
  (parse-body '("Docstring"
                (declare (ignorable x))
                "Not Docstring"
                form1 form2
                (declare (ignore y)) form3) :strict nil)
  (assert (equal body '("Not Docstring" form1 form2)))
  (assert (equal decl '((declare (ignorable x))
                        (declare (ignore y)))))
  (assert (string= docs "Docstring")))

; l!
(assert (listp (l! :hello)))

(assert (equal (l! 1 2 3) '(1 2 3)))

(assert (equal (l! '(1 2) 3) '(1 2 3)))

(assert (equal (l! 1 '(2) 3 '(4 5)) '(1 2 3 4 5)))

(assert (equal (l! #'1+ '(0 1 2) 4) '(1 2 3 4))

(assert (equal (l! '(1 ~a ~a 3) 2 ) '(1 2 3)))


; flatten
(assert (equal (flatten '(a (b (c (d ,:e) f) (g)) h))
               '(a b c d :e f g h)))


; s!
(assert (stringp (s! :hello)))

(assert (string= (s! "Hello " "World") "Hello World")) 

(assert (string= (s! 'hello " " :world) "HELLO WORLD"))

(assert (string= (s! "Hello, ~a!" :world " " "Hi!") "Hello, WORLD Hi!"))


; sy!
(assert (symbolp (sy! :hello)))

(assert (string= (symbol-name (sy! :hello)) (symbol-name 'hello)))

; k!
(assert (keywordp (k! :hello)))

(assert (string= (symbol-name (k! :hello)) (symbol-name 'hello)))

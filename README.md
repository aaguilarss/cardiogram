# Cardiogram 🧡

A framework for impromptu testing in Common Lisp.

WARNING: Still in alpha.

## Usage

The main objects in cardiogram are tests. To define a test use the
`deftest` macro. It's syntax is:

```
(deftest <name> (<options>*) <docstring>* <form>*)

<options> := | :before <symbol> | <list-of-symbols>
             | :after <symbol> | <list-of-symbols>
             | :around <symbol> | <list-of-symbols>
             | :depends-on <dependency-c-d>
             | :time-limit <number>

<dependency-c-d> := ([or and] [<symbol> <dependency-c-d>]+)
```

To run a test, call it by name. For example:

```common-lisp
(deftest myfunction-test ()
  (true (myfunction))


(myfunction-test)
;=> FAILED - TRUE.

```

This will run the code inside the test, then save the result. The second time you call `(a)` the
code won't be run, rather the test result will be printed. To run the test again, call it with
the `:run` option like so:

```common-lisp
(redefine-myfunction)

(myfunction-test :run)

;=> PASSED - TRUE

```

#### Chaining tests

You can combine tests using the `:before`, `:around`, and `:after` options. For example:

```common-lisp
(deftest myotherfunction-test (:after myfunction-test)
  (false (myotherfunction (myfunction)))

(deftest myvariable-test (:around (myfunction-test myotherfunction-test))
  (of-type myvariable 'string))

(myfunction-test :run)

;=> Running test myvariable-test...
;   Running test myfunction-test...
;   Running test myvariable-test...
;   Running test myotherfunction-test...
;   Running test myvariable-test...

(myotherfunction-test :run)

;=> Running test myvariable-test...
;   Running test myotherfunction-test...
;   Running test myvariable-test...

(myvariable-test :run)

;=> Running test myvariable-test...
```

You can also tell a test to skip any test in it's combination by specifying it's name
when you call it:

```common-lisp
... FAILED - MYVARIABLE-TEST

(myfunction-test :run :skip `(myvariable-test))


;=> Running test myfunction-test...
;   Running test myotherfunction-test...
```

#### Test dependencies

To define dependencies for a test, use the `:depends-on` option.

```common-lisp
(deftest myotherfunction-test (:after myfunction-test :depends-on (myvariable-test))
  ...)

(myfunction-test :run)

;=> Running test myvariable-test...
;   Skipping myfunction-test. Failed dependencies.

```

Additionally you can use the `:dependency-of` option to add the test being defined to
another's dependencies.

```common-lisp

(deftest myotherfunction-test (:dependency-of myfunction-test)
  ...)

(myfunction-test)

;=> Running test myotherfunction-test
; Test myotherfunction-test failed
; ... Dependency error... skipping test myfunction-test
```

Both `:dependency-of` and `:depends-on` accept dependency disjunctions and conjunctions
of the form:

```common-lisp
<dependency-expr> := (<operator-keyw> <symbol> <dependency-expr>)
<operator-keyw> := :and :or
```

Tests are funcallable, so you can programatically call tests with Lisp's own `funcall`. For example:

```common-lisp
(loop for sy being each symbol in *package* doing
      (when (tboundp sy)
        (funcall (symbol-test sy))))
```

Furthermore, being functions, tests will return `t` or `nil` whenever they pass or fail respectively.


```common-lisp
; silly example

(when (myfunction-test)
  (asdf:make :mypackage))

```

#### Errors

A global variable called `*ignore-errors*` controls if a test invokes the debugger on error or not.
It's set to `nil` by default. When set to `t`, errors will be ignored but sill be reported. A test
with an error is a failed test.

```common-lisp

(setf *ignore-errors* t)

(deftest a ()
(+ 'a 1))

(a)

;=> Running test A...
;   Test A took 0.0s to run.
;   Test A FAIL
;
;   Value of 'A in (+ 'A 1) is A, not a NUMBER. 'A(+ 'A 1)NUMBER
;   NIL
```

## Comparisons, valuations and formats.

Cardiogram provides the following valuations to be used inside a test's forms.

```
(true form)

(false form)

(fail form)

(pass form)

(is form expected)

(isnt form expected)

(is-values form expected)

(isnt-values form expected)

(is-print form expected)

(eql-types form1 form2)

(of-type form expected)

(expands-1 form expected)
```

You can define new valuations by a two step process. First use the `define-valuation` macro. The body in
`define-valuation` corresponds to the test used when calling a valuation. It should
return true or false.

```common-lisp
; (define-valuation valuation-name args &body body)

(define-valuation my-is (form expected)
  (my-eql form expected))
```
Next you need to define a reporter function for your valuation. To do this, use the `define-format` macro.
The body in this macro should be either a string or the body of a function taking two arguments.
The first argument is a list of the arguments passed to the valuation. The second is the result of
the valuation. When defining a function. It should return a report string.

```common-lisp
; (define-format valuation-name format-name args &body body)

(define-format my-is simple (args result)
  (with-output-to-string (s)
    (destructuring-bind (form expected) args
      (if result (princ form s) (princ "NOPE" s))))
```

The format `simple` is the default format in cardiogram and `binary` is the fall-back format.
You can define new formats by individually
adding your format to each valuation name. Then to use it do `(setf *default-format* 'myformat)`.

```common-lisp
(ql:quickload :alexandria)
(ql:quickload :cl-yaml)

(define-format fail myformat (args result)
  (declare (ignore args result))
  (yaml:emit-to-string
    (alexandria:alist-hash-table '(("result" . "failed")
                                   ("reason" . "Always Fails"))))

```

Cardiogram outputs report strings to a stream called `*test-output*`, which defaults to `*standard-output*`.
You can change it to whatever stream you like.



## Fixes

Sometimes you need to run a test that changes an aspect of the environment. To fix the environment again,
cardiogram provides a few macros wrapping `unwind-protect`. The main one is `with-fixes`.

```common-lisp

var1
;=> 4

var2
;=> 3

(with-fixes (var1 var2)
  ...
  (setf var1 3)
  (setf var2 4) ...)

var1
;=> 4

var2
;=> 3

```

#### Environments with automatic fixes

Also there are macros that automatically compute fixes for symbols whose `symbol-name`
starts with `F!`. They are `f!let`, `f!let*`, `f!block` and `f!labels`

```common-lisp
*global-var*

;=> 4

(f!let (...)
  (setf *global-var* 'symbol)
  (setf f!*global-var* "something else"))

*global-var*

;=> 4

```

Notice how preppending the variable name anywhere inside the `f!let` is sufficient.

You can define your own fixes with the `defix` macro.

```common-lisp
; (defix name args &body body)
```

Copyright (c) 2018 Abraham Aguilar (a.aguilar@ciencias.unam.mx)

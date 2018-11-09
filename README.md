# Cardiogram

A simple test framework for common lisp. Inspired by parachute.

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

## Chaining tests

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

To define dependencies for a test, use the `:depends-on` option. It will take a list of
symbols designating test names and logical operators.

```common-lisp

(deftest myotherfunction-test (:after myfunction-test :depends-on (myvariable-test))
  ...)

(myfunction-test :run)

;=> Running test myvariable-test...
;   Skipping myfunction-test. Failed dependencies.

```

## Comparisons and valuations

Cardiogram provides the following valuations:

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

The built-in behaviour of all theese returns true when passed or nil when failed
as primary values. As secondary values a string with a message.


Valuation behaviour and reporting can be customized.


## Fixes

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

There are a few macros that automatically compute fixes for symbols whose `symbol-name`
starts with `F!`. They are `f!let`, `f!let*` and `f!labels`

```common-lisp
*global-var*

;=> 4

(f!let (...)
  (setf f!*global-var* 'symbol)
  (setf *global-var* "something else"))

*global-var*

;=> 4

```



## Installation

Copyright (c) 2018 Abraham Aguilar (a.aguilar@ciencias.unam.mx)

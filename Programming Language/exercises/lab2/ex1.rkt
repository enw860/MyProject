#lang racket #| CSC324 Winter 2018: Exercise 1 |#
#|
★ Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html ★

In this part of the exercise, you'll get started writing some simple
functions in Racket. Since this is likely your first time using Racket,
we strongly recommend going through some of the documentation we listed
under the "Software" page as you work through this exercise.
In comments below, we also give some links to documentation to built-in
functions for standard data types (numbers, strings, lists) that we want
you to become familiar with.

Finally, you'll notice the (module+ test ...) expressions interleaved with
the function definitions; this is a standard Racket convention for simple
unit tests that we'll use throughout the course. Please read them carefully,
and add tests of your own!
|#
;-------------------------------------------------------------------------------
; This expression exports functions so they can be imported into other files.
; Don't change it!
(provide celsius-to-farenheit n-copies num-evens num-many-evens)

; We use (module+ test ...) to mark code that shouldn't be evaluated when this
; module is imported, but instead is used for testing purposes (similar to Python's
; if __name__ == '__main__').

; Right now each module+ expression after this one (i.e., the ones that actually
; contain test cases) is commented out, because they all fail, and DrRacket runs
; tests automatically when you press "Run".
; As you do your work, uncomment the module+ expression by deleting the `#;` in
; front of the module+ expressions and run the module to run the tests.
;
; NOTE: As is common to testing frameworks, by default DrRacket only displays
; output for *failing* tests. If you run the module with the tests uncommented
; but don't see any output, that's good -- the tests all passed! (If you want
; to double-check this, you can try breaking test cases and seeing the "fail"
; output yourself.)
(module+ test
  ; Import the testing library
  (require rackunit))

;-------------------------------------------------------------------------------

#|
(celsius-to-farenheit temp)
  temp: an integer, representing a temperature in degrees Celsius

  Returns the equivalent temperature in degrees Farenheit, rounded to the
  nearest integer.

  Relevant documentation: https://docs.racket-lang.org/reference/generic-numbers.html.
|#
(define (celsius-to-farenheit temp)
  ; TODO: replace the (void) with a proper function body.
  (exact-round (+ 32 (* temp 1.8))))

(module+ test
  ; We use rackunit's check-equal? to define some simple tests.
  (test-equal? "(celsius-to-farenheit 0)"  ; Test label
               (celsius-to-farenheit 0)    ; Actual value
               32)                         ; Expected value
  (test-equal? "(celsius-to-farenheit 14)"
               (celsius-to-farenheit 14)
               57)
  (test-equal? "(celsius-to-farenheit -100)"
               (celsius-to-farenheit -100)
               -148)
  (test-equal? "(celsius-to-farenheit 37)"
               (celsius-to-farenheit 37)
               99)
  (test-equal? "(celsius-to-farenheit 38)"
               (celsius-to-farenheit 38)
               100))

;-------------------------------------------------------------------------------

#|
(n-copies s n)
  s: a string
  n: a non-negative integer n

  Returns a string consisting of n copies of s.
  *Use recursion!* Remember that you aren't allowed to use mutation for this exercise.

  Relevant documentation: https://docs.racket-lang.org/reference/strings.html
|#
(define (n-copies s n)
  (if (= n 0) ""
      (string-append s (n-copies s (- n 1)))))

(module+ test
  (test-equal? "n-copies: Three copies"
               (n-copies "Hello" 3)
               "HelloHelloHello")
  (test-equal? "n-copies: Zero copies"
               (n-copies "Hello" 0)
               "")
  (test-equal? "n-copies: Single letter"
               (n-copies "a" 10)
               "aaaaaaaaaa"))

;-------------------------------------------------------------------------------

#|
(num-evens numbers)
  numbers: A list of integers.

  Returns the number of even elements in the list.

  Relevant documentation: https://docs.racket-lang.org/reference/pairs.html.

  Reminder: do not use mutation or loop constructs here.
  Instead, use the basic *recursive* template on lists, which we've started
  for you in the commented code below.
|#
(define (num-evens numbers)
  (cond
    [(null? numbers) 0]
    [else
     ; In this case the list is non-empty. Divide the list into its first
     ; element and the remaining numbers in the list, recurse on the remaining,
     ; and combine the results in some way to return the final result.
     (let ([first-number (first numbers)]
           [remain-numbers (num-evens (rest numbers))])  ; Pick a better variable name.
       (cond [(even? first-number) (+ 1 remain-numbers)]
             [else (+ 0 remain-numbers)]
       ))])
  )

#|
(num-many-evens lists-of-numbers)
  lists-of-numbers: A list of lists of integers.

  Return the number of inner lists that contain three or more even integers.

  Hint: you can use a very similar approach to the previous exercise.
|#
(define (num-many-evens lists-of-numbers)
  (cond
    [(null? lists-of-numbers) 0]
    [else
     (let ([first-inner-list (first lists-of-numbers)]
          [remain-inner-lists (num-many-evens (rest lists-of-numbers))])
       (cond [(> (num-evens first-inner-list) 2) (+ 1 remain-inner-lists)]
             [else (+ 0 remain-inner-lists)])
      )]))

(module+ test
  (test-equal? "num-evens: empty list"
               (num-evens null)
               0)
  (test-equal? "num-evens: simple non-empty list"
               (num-evens (list 1 2 4))
               2)
  
  (test-equal? "num-many-evens: empty list"
               (num-many-evens null)
               0)
  (test-equal? "num-many-evens: simple non-empty list"
               (num-many-evens (list (list 2 4 5 7 8)))
               1))

#lang racket #| CSC324 Winter 2018: Lab 5 |#

;-------------------------------------------------------------------------------
; ★ Task 1: Practice with macros ★
;-------------------------------------------------------------------------------
#|
(my-or p q)
  p, q: booleans

  Equivalent to (or p q) in its short-circuiting behaviour.

  Note: the built-in `or` (and other forms) accept non-boolean arguments
  and do some implicit type conversion that you can read about online.
  Don't worry about that for the purpose of this exercise.
|#
(define-syntax my-or
  (syntax-rules ()
    [(my-or <p> <q>)
     (if <p> #t <q>)]))


#|
(my-and p q)
  p, q: booleans

  Equivalent to (and p q).
|#
(define-syntax my-and
  (syntax-rules ()
    [(my-and <p> <q>)
     (if <p> (if <q> #t #f) #f)]))


#|
(my-or* p ...)
  p ... : booleans

  A generalization of my-or to take in an arbitrary number
  of boolean arguments.
  It should behave the same as the built-in `or` (for boolean arguments).

  This macro should be *recursive* (i.e., use itself in a template).
  We've illustrated the basic pattern-matching forms below.
|#
(define-syntax my-or*
  (syntax-rules ()
    [(my-or*)
     ; Zero arguments. What does (or) return?
     #f]
    [(my-or* <first> <rest> ...)
     ; <first> matches the first argument.
     ; <rest> ... matches the other arguments (note the ellipsis)
     (if <first> #t (my-or* <rest> ...))]))


#|
(my-and* p ...)
  p ... : booleans

  A generalization of my-or to take in an arbitrary number
  of boolean arguments.
  It should behave the same as the built-in `and` (for boolean arguments).
|#
(define-syntax my-and*
  (syntax-rules ()
   [(my-and*) #t]
   [(my-and* <p> <q> ...)
    (if <p> (my-and* <q> ...) #f)]))


#|
(my-cond
  [test-or-else then] ...)

  Behaves like the built-in `cond`, restricted to the forms where `test-or-else`
  is either an expression that returns a boolean (e.g., (> 3 4)) or the literal `else`.
  The built-in `cond` requires that `else` appear last; your macro should enforce
  the same syntactic constraint.

  How do you make `else` a literal keyword?
  If you don't remember from lecture, ask your TA!

  See https://docs.racket-lang.org/reference/if.html for details.
|#
(define-syntax my-cond
  (syntax-rules ()
    [(my-cond <condition> ) <condition>]))


;-------------------------------------------------------------------------------
; ★ Task 2: Extending the `my-class` macro ★
;-------------------------------------------------------------------------------
; You may find it useful to use the `hash-union` function here.
; Note that it raises an error when there's duplicates, though;
; for the purpose of this lab you can assume that there are no duplicates
; between *instance attribute names* (set by the constructor) and
; *method names*.
(require racket/hash)

(define (attribute-error object attr)
  (lambda () (error (format "~a has no attribute ~a." object attr))))

(define-syntax my-class
  ; Note that `new` is now a literal keyword.
  (syntax-rules (method new)
    [(my-class <Class>
       ; This is a *required* method that must appear first in the class body.
       ; Also, there's no more explicit list of attributes.
       (method (new <new-param> ...) <new-body>)

       ; You can still define other methods, but they won't be very useful
       ; without `self`.
       (method (<method-name> <param> ...) <body>) ...)

     ; TODO: Complete the following template.
     ; There are many different ways of supporting the required functionality,
     ; but a suggested approach is the following:
     ;   1. Create a dictionary of attributes based on the constructor.
     ;   2. Create a dictionary of the other methods. Don't include `new`.
     ;   3. Merge the two dictionaries into the final __dict__ bindings, so that
     ;      you can keep the same `hash-ref` call from lecture.
     (define (<Class> arg . args)
       (lambda (msg)
         (let ([__dict__ (hash-union
                         (apply (lambda (<new-param> ...) <new-body>)
                                (cons arg args))
                         (make-immutable-hash
                         (list  (cons (quote <method-name>)
                                      (lambda (<param> ...) <body>))
                                ...
                         )))])
          (hash-ref __dict__ msg
                    (attribute-error (quote <Class>) msg)))))
     ]

    ; Macro pattern from lecture, provided for your reference.
    ; Don't change this!
    [(my-class <Class> (<attr> ...)
       (method (<method-name> <param> ...) <body>) ...)

     (define (<Class> <attr> ...)
       (lambda (msg)
         (let ([__dict__
                (make-immutable-hash
                 (list (cons (quote <attr>) <attr>)
                       ...
                       (cons (quote <method-name>)
                             (lambda (<param> ...) <body>))
                       ...
                       ))])

           ; Look up the given attribute in the object's dictionary.
           (hash-ref __dict__ msg
                     ; Raise an error if not attribute not found.
                     (attribute-error (quote <Class>) msg)))))]
    
    ))


; Sample usage: uncomment when ready!

(my-class Point
  (method (new x y)
          (hash 'x x
                'y y
                'z 0
                'extra (+ x y)))

  ; Simple methods just to ensure the syntax is okay.
  ; We'll support "real" methods in lecture this week.
  (method (return-50) 50)
  (method (return-100) 100))


(define p (Point 2 3))  ; `Point` has the same signature as `new`.
(p 'x)                  ; 2
(p 'y)                  ; 3
(p 'z)                  ; 0
(p 'extra)              ; 5
((p 'return-50))        ; Methods are still supported
((p 'return-100))       ; Methods are still supported


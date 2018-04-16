#lang racket #| CSC324 Winter 2018: Exercise 2 |#
#|
★ Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html ★
|#
;-------------------------------------------------------------------------------
; This expression exports functions so they can be imported into other files.
; Don't change it!
(provide tail-calls call-order)

(define (isOperator x)
  (cond
    [(equal? x '+) #t]
    [(equal? x '-) #t]
    [(equal? x '*) #t]
    [(equal? x '/) #t]
    [else #f]))

(define (isAndOr x)
  (cond
   [(equal? x 'or) #t]
   [(equal? x 'and) #t]
   [else #f]))

#|
(tail-calls expr)
  expr: A racket datum with the structure defined on the exercise handout.

  Return a list of function call expressions that are in tail call position
  with respect to the input expression.
|#
(define (tail-calls expr)
  (cond
    [(null? expr) null]
    [(not (list? expr)) null]
    [(equal? (length expr) 1) null]
    [else
     (let ([last-element (last expr)]
           [last-call (tail-calls (last expr))]
           [first-element (first expr)])
       (cond
         [(equal? first-element 'if)
          (if (null? (tail-calls (third expr)))
              (if (null? last-call) null
                  last-call)
              (remv* (list null) (append (list (third expr)) last-call)))]
         [(isAndOr first-element)
          (if (null? last-call) null (list last-element))]
         [(isOperator first-element) (list expr)]
         [(null? last-call) (list expr)]
           (list last-element)))]))

(module+ test
  (require rackunit)
  ; We've provided test cases for the first two syntactic forms described
  ; in the handout. Please add additional test cases, including ones for the
  ; other forms!
  (test-equal? "test if1" (tail-calls '(if #t 1 (equal? 1 1))) (list '(equal? 1 1)))
  (test-equal? "test if2" (tail-calls '(if #t (equal? 1 1) 99)) (list '(equal? 1 1)))
  (test-equal? "test if3" (tail-calls '(if #t 1 1)) null)
  (test-equal? "test or1" (tail-calls '(or 1 1 1 1 1 (+ 1 2))) (list '(+ 1 2)))
  (test-equal? "test or2" (tail-calls '(or 1 1 1 1 1 (+ 1 2) 1)) null)
  (test-equal? "test and" (tail-calls '(and 1 1 1 1 1 (+ 1 2) (isOperator 'a))) (list '(isOperator 'a)))
  (test-equal? "Atomic value" (tail-calls 3) null)
  (test-equal? "Simple call" (tail-calls '(+ 1 2)) (list '(+ 1 2)))
  (test-equal? "Nested call"
               (tail-calls '(+ (* 3 4) 2))
               ; NOTE: the outermost expression is in tail-call position,
               ; and it should just be output directly. Don't try to evaluate
               ; the inner '(* 3 4) -- this is harder to do!
               (list '(+ (* 3 4) 2))))


#|
(call-order expr)
  expr: A Racket datum consisting only of atoms and function calls.

  Return a list of Racket data that are the function call expressions in `expr`,
  in the order in which they would be called using left-to-right eager evaluation.
  Every element in the returned list should be a quoted expression that appears
  literally in `expr` -- don't worry about simplifying nested function calls.

  NOTE: The actual implementation turns out to be pretty simple, and should remind
  you of *tree traversals*.
|#
(define (call-order expr)
  (cond
    [(null? expr) null]
    [(not (list? expr)) null]
    [(equal? (length expr) 1) null]
    [(equal? (length (filter list? expr)) 0) (list expr)]
    [else
     (let ([result (checkSub expr)])
       (remv* (list null) result))]))


(define (checkSub expr)
  (cond
    [(not (list? expr)) null]
    [else
     (let ([result (filter list? expr)])
       (if (equal? (length result) 0) expr
           (append (map checkSub result) (list expr))))]))

(module+ test
  ; Again, this is a pretty incomplete set of tests! One of the skills you're developing
  ; in this course is how to develop your own test case inputs, often motivated by the
  ; *structure* of the input data.
  (test-equal? "Atomic value" (call-order 3) null)
  (test-equal? "Simple call" (call-order '(+ 1 2)) (list '(+ 1 2)))
  (test-equal? "One nested call"
               (call-order '(+ (* 3 4) 2))
               ; NOTE: the second call is expressed as '(+ (* 3 4) 2), the original
               ; expression itself, rather than what would "actually" be evaluated,
               ; '(+ 12 2). This simplification should make your job easier. :)
               (list '(* 3 4)
                     '(+ (* 3 4) 2)))
  (test-equal? "Complex expression"
               (call-order '(+ (1 (* 1 3 4) (+ 2 2 4)) 2))
               (list '((* 1 3 4) (+ 2 2 4) (1 (* 1 3 4) (+ 2 2 4)))
                     '(+ (1 (* 1 3 4) (+ 2 2 4)) 2))))

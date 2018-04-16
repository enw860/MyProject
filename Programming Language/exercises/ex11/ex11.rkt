#lang racket #| CSC324 Winter 2018: Exercise 8 |#
#|
★ Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html ★
|#
(provide Nothing Just
         safe-div parse-num
         bind return do-maybe)


;-------------------------------------------------------------------------------
; Task 1: Modeling Maybe in Racket
;-------------------------------------------------------------------------------
(define Nothing 'Nothing)
(define (Just x) (list 'Just x))


#|
(safe-div x y)
  x: int
  y: int

  If y equals 0, returns Nothing.
  Else returns (quotient x y)---wrapped in a Just, of course!
|#
(define (safe-div x y)
  (if (equal? y 0) Nothing (Just (quotient x y))))

#|
(parse-num s)
  s: a string

  If an int can be parsed from s by using `string->number`, then
  succeed with that int. Else fail and return Nothing.
|#
(define (parse-num s)
  (let ([result (string->number s)])
    (if result (Just result) Nothing)))


; Equivalents of `return` and `(>>=)` for Maybe.
; (bind x f) should be equivalent to x >>= f in Haskell.
; Make sure you get the type signature for (>>=) correct from Haskell!
(define return
  (lambda (x)
    (if (equal? x Nothing) Nothing (Just x))))

(define (bind x f)
  (if (equal? x Nothing) Nothing 
      (let ([val (second x)])
        (f val))))

;-------------------------------------------------------------------------------
; Tasks 2 and 3: do notation in Racket
;-------------------------------------------------------------------------------
(define-syntax do-maybe
  (syntax-rules (do-let <-)
    [(do-maybe <expr>) <expr>]
    
    [(do-maybe (do-let <var> <val>) <expr> ...)
     (let ([<var> <val>])
       (do-maybe <expr> ...))]
    
    [(do-maybe (<id> <- <expr>) <expr2> ...)
     (bind <expr> (lambda (<id>) (do-maybe <expr2> ...)))]
    
    [(do-maybe <expr1> <expr2> ...)
     (bind <expr1> (lambda (| |) (do-maybe <expr2> ...)))]))
    
; Sample tests for Task 2
(module+ test
  (require rackunit)

  (test-equal?
   "Simple do expression"
   (do-maybe
    (x1 <- (parse-num "3"))
    (x2 <- (parse-num "8"))
    (return (+ x1 x2)))
   (Just 11))

  (test-equal?
   "Simple do expression with Nothing"
   (do-maybe
    (x1 <- (parse-num "3"))
    (x2 <- (parse-num "david"))
    (return (+ x1 x2)))
   Nothing))


; Sample tests for Task 3
(module+ test
  (require rackunit)

  (test-equal?
   "Do notation without explicit <-"
   (do-maybe
    (Just 1)
    (Just 2)
    (Just 3)
    (Just 4))
   (Just 4))

  (test-equal?
   "Do notation without explicit <-, with a Nothing"
   (do-maybe
    (Just 1)
    (Just 2)
    Nothing
    (Just 4))
   Nothing)

  (test-equal?
   "Using let to bind a pure value"
   (do-maybe
    (do-let x 50)
    (do-let y 8)
    (safe-div x y))
   (Just 6))

  (test-equal?
   "Mixing let and bind"
   (do-maybe
    (x <- (parse-num "3"))
    (do-let y (+ x 10))
    (return y))
   (Just 13))

  (test-equal?
   "Mixing let and bind"
   (do-maybe
    (x <- (parse-num "david"))
    (do-let y (+ x 10))
    (return y))
   Nothing))

#lang racket #| CSC324 Winter 2018: Lab 6 |#

;-------------------------------------------------------------------------------
#| ★ Task 0: Lazy list implementation ★
;-------------------------------------------------------------------------------
This section defines an implementation of streams using lazy lists.
The key idea is in s-cons, which is exactly analogous to Racket's cons,
except that it wraps its "rest" value in a *thunk* so that it is only
evaluated when accessed, rather than when it's passed to s-cons,
as it would be during standard eager evaluation order.

List value/function   Stream analogue
-------------------   ---------------------
null (empty list)     s-null (empty stream)
null?                 s-null?
cons                  s-cons
first                 s-first
rest                  s-rest
|#

(define s-null 's-null)
(define (s-null? stream) (equal? stream 's-null))


#|
(thunk <expr>)
  <expr>: A Racket expression. (*Not* a datum, the actual expression.)

  Rewrites this into a thunk (i.e., nullary function) that wraps <expr>.
  As we saw in lecture, this is used to delay evaluation of <expr>.
  (This is actually a built-in macro, but we've provided an implementation
  here to be explicit.)
|#
(define-syntax thunk
  (syntax-rules ()
    [(thunk <expr>) (lambda () <expr>)]))


#|
(s-cons <first> <rest>)
  <first>: A Racket expression.
  <rest>: A stream (e.g., s-null or another s-cons expression).

  Returns a new stream whose first value is <first>, and whose other items
  are the ones in <rest>. Unlike a regular list, <rest> is wrapped in a thunk,
  so it isn't evaluated until called (see s-rest below).
|#
(define-syntax s-cons
  (syntax-rules ()
    [(s-cons <first> <rest>)
     (cons <first> (thunk <rest>))]))

; These two define the stream-equivalents of "first" and "rest".
; We need to use `car` and `cdr` here for a technical reason that
; isn't important.
; Note that s-rest both accesses the "rest thunk" and calls it,
; so that it does indeed return a stream.
(define (s-first stream)(car stream))
(define (s-rest stream) ((cdr stream)))


(module+ test
  ; TODO: Fill in all the ... below with relevant stream values/functions
  ; so that all the tests pass.

  (require rackunit)

  (test-true "Create an empty stream and check that it's empty."
             (s-null? 's-null))
  (test-false "Create a stream of length 1 and check that it's non-empty."
              (s-null? (s-cons 1 's-null)))

  (test-equal? "Create a stream of length 1 containing the number 165 and access the item."
               (s-first (s-cons 165 's-null))
               (first (cons 165 null)))

  (test-equal? "Create a stream of length 3 containing the numbers 4, 5, 6 and access the third item."
               (s-first (s-rest (s-rest (s-cons 4 (s-cons 5 (s-cons 6 's-null))))))
               6))

;-------------------------------------------------------------------------------
; ★ Task 1: Lazy list functions ★
;-------------------------------------------------------------------------------
#|
(s-sum numbers)
  numbers: a stream of numbers

  Returns the sum of the numbers in the stream.
|#
(define (s-sum numbers)
  (if (s-null? numbers) 0
      (+ (s-first numbers) (s-sum (s-rest numbers)))))


#|
(s-sum-tail numbers acc)

  A tail-recursive version of s-sum.
|#
(define (s-sum-tail numbers acc)
  (if (s-null? numbers) acc
      (s-sum-tail (s-rest numbers)
      (+ acc (s-first numbers)))))


#|
(s-member v stream)

  Analogous to Racket's `member` list function.
  (Note that when `v` is in the stream, this returns a stream!

  See https://docs.racket-lang.org/reference/pairs.html.
|#
(define (s-member v stream)
  (if (s-null? stream) #f
      (if (equal? v (s-first stream)) (s-rest stream)
          (s-member v (s-rest stream)))))


#|
(stream->list stream)
  stream: A stream

  Returns a list containing the values in this stream.
|#
(define (stream->list stream)
  (if (s-null? stream) (list)
      (list* (s-first stream) (stream->list (s-rest stream)))))


#|
(s-range start end)
  start, end: integers.

  Precondition: start <= end

  Returns a stream containing the numbers start, start+1, ..., end-1.
  Returns an empty stream if (equal? start end).
|#
(define (s-range start end)
  (if (equal? start end) 's-null
      (s-cons start (s-range (+ 1 start) end))))


#|
(s-take stream n)
  stream: A stream
  n: A non-negative integer

  Returns a new stream that contains the first n elements of `stream`,
  or all of the elements of `stream` if it has fewer than n elements.
|#
(define (s-take stream n)
  (if (equal? n 0) 's-null
      (s-cons (s-first stream)
              (if (s-null? (s-rest stream)) 's-null
                  (s-take (s-rest stream) (- n 1))))))


;-------------------------------------------------------------------------------
; ★ Task 2: Infinite lists ★
;-------------------------------------------------------------------------------

#|
Hints:
  1. These functions should all be explicitly recursive (i.e., call themselves).
     It's up to you to figure out how, exactly.
  2. You can test your work using `s-take` and `stream->list` from above.
|#


#|
(repeat x)
  x: A value

  Returns an infinite stream whose elements are all equal to `x`.
|#
(define (repeat x)
  (s-cons x (repeat x)))

#|
(range-to-infinity n)
  n: An integer

  Returns an infinite stream of the integers n, n+1, n+2, ...
|#
(define (range-to-infinity n)
  (s-cons n (range-to-infinity (+ 1 n))))


#|
(cycle x1 ...)
  x1 ...: A value

  Takes an arbitrary, but non-zero, number of values,
  and returns an infinite stream whose elements are the arguments to cycle,
  repeated infinitely often.

  For example, (cycle 1 2 3) should return the stream `1,2,3,1,2,3,1,2,3,...`
|#
(define (s-build-list args org)
  (if (null? args) (s-build-list org org)
      (s-cons (first args)
              (s-build-list (rest args) org))))

(define (cycle arg . args)
  (s-build-list null (cons arg args)))

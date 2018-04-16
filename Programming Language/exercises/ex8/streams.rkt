#lang racket #| CSC324 Winter 2018: Stream implementation |#

(provide thunk
         s-null
         s-null?
         s-cons
         s-first
         s-rest
         s-range
         stream->list)


; Empty stream value, and check for empty stream.
(define s-null 's-null)
(define (s-null? stream) (equal? stream 's-null))


#|
(thunk <expr>)
  <expr>: A Racket expression. (*Not* a datum, the actual expression.)

  Rewrites this into a thunk (i.e., nullary function) that wraps <expr>.
  As we saw in lecture, this is used to delay evaluation of <expr>.
  (This is actually a built-in macro, but we've provided an implementation
  here to be explicit.)

  NOTE: this is slightly modified from lab, in taht it accepts more than one
  expression for the function body. This doesn't come up in pure functional
  programming, but we'll use it in an impure (mutation-involving) way in lecture.
|#
(define-syntax thunk
  (syntax-rules ()
    [(thunk <expr> ...) (lambda () <expr> ...)]))


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
(define (s-first stream) (car stream))
(define (s-rest stream) ((cdr stream)))


#|
(stream->list stream)
  stream: A stream

  Returns a list containing the values in this stream.
|#
(define (stream->list stream)
  (if (s-null? stream)
      null
      (cons (s-first stream) (stream->list (s-rest stream)))))


#|
(s-range start end)
  start, end: integers.

  Precondition: start <= end

  Returns a stream containing the numbers start, start+1, ..., end-1.
  Returns an empty stream if (equal? start end).
|#
(define (s-range start end)
  (if (equal? start end)
      s-null
      (s-cons start
              (s-range (+ 1 start) end))))


#|
(s-take stream n)
  stream: A stream
  n: A non-negative integer

  Returns a new stream that contains the first n elements of `stream`,
  or all of the elements of `stream` if it has fewer than n elements.
|#
(define (s-take stream n)
  (if (equal? n 0)
      s-null
      (s-cons (s-first stream)
              (s-take (s-rest stream) (- n 1)))))

#lang racket #| CSC324 Winter 2018: Lab 4 |#

;-------------------------------------------------------------------------------
; ★ Task 1: Closures as objects ★
;-------------------------------------------------------------------------------
#|
(Point x y)
  x: an integer (representing the x-coordinate of the point)
  y: an integer (representing the y-coordinate of the point)

  Returns a function representing the *point* (x, y).
  Read the given code carefully to see how the returned function expects to
  be called, and experiment in the interpreter!

|#
(define (Point x y)
  (lambda (attr)
    (cond
      [(equal? attr 'x) x]
      [(equal? attr 'y) y]
      [(equal? attr 'scale) (lambda (t) (Point (* t x) (* t y)))]
      [else (error (format "Point has no attribute ~a." attr))])))

; add two coods of a point
(define (sum-of-cood p)
  (if (not (procedure? p)) (error (format "in put not a Point"))
      (+ (p 'x) (p 'y))))

; distance between two points
(define (distance p1 p2)
  (let ([x (- (p1 'x) (p2 'x))]
        [y (- (p1 'y) (p2 'y))])
    (sqrt (+ (* x x) (* y y)))))

; a list of point according input n
(define (nPoints n)
  (if (> n 1) (cons (nPoints (- n 1)) (Point (- n 1) (- n 1)))
      (Point 0 0)))

(define p1 (Point 3 4))
(define p2 (Point 0 0))

(sum-of-cood p1) ;should be 7
(distance p1 p2) ;should be 5

(nPoints 3)

(define p3 ((p1 'scale) 2))
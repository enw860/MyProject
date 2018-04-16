#lang racket

; Return a function that raises an attribute error (with an appropriate message).
(define (attribute-error object attr)
  (lambda () (error (format "~a has no attribute ~a." object attr))))

(define-syntax my-class
  (syntax-rules (method)
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
                     (attribute-error (quote <Class>) msg)))))]))


(my-class Point
  (x y)

  (method (size)
          (sqrt (+ (* x x) (* y y))))

  (method (scale n)
          (Point (* x n) (* y n)))
  )


(define p (Point 2 3))
(format "p: (~a, ~a)" (p 'x) (p 'y))
(p 'size)
((p 'size))
(define p2 ((p 'scale) 5))
(format "p2: (~a, ~a)" (p2 'x) (p2 'y))

(my-class Person
  (name age likes-chocolate))

(define david (Person "David" 100 #true))
(format "~a: age ~a. Likes chocolate: ~a."
        (david 'name) (david 'age) (david 'likes-chocolate))

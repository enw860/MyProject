#lang racket #| CSC324 Winter 2018: Exercise 5 |#
#|
★ Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html ★
|#
;-------------------------------------------------------------------------------
(provide my-class-getter my-class-setter)


; Return a function that raises an attribute error (with an appropriate message).
(define (attribute-error object attr)
  (lambda () (error (format "~a has no attribute ~a." object attr))))


#| Task 1: Accessor functions |# 

#|
(my-class-getter <Class> (<attr> ...)
  (method (<method-name> <param> ...) <body>) ...)

  This macro accepts the *exact* same pattern as my-class from lecture,
  and defines a constructor function that behaves in the same way as well.

  In addition to defining the constructor, my-class-getter defines
  *one new accessor function per attribute of the class*, using the name
  of the attribute as the name of the function.

  Implementation notes:
    - Our starter code has wrapped the `define` from lecture inside a `begin`.
      This is required so that you can add more `define`s after the one for the
      constructor.
|#
(define-syntax my-class-getter
  (syntax-rules (method)
    [(my-class-getter <Class> (<attr> ...)
       (method (<method-name> <param> ...) <body>) ...)

     (begin
       ; This is the constructor from lecture.
       ; You may change this, but it is possible to complete this task without
       ; touching this part at all.
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
                       (attribute-error (quote <Class>) msg)))))
       
       ; You can add more definitions here.
       (define <attr> (lambda (pt) (pt (quote <attr>))))
       ...
       
       )]))


(module+ test
  (require rackunit)

  ; We use `local` to create a local scope for our definitions.
  ; Run these tests when you're ready!
  (local
    [(my-class-getter Point (x y)
       (method (size)
               (sqrt (+ (* x x) (* y y))))

       (method (scale n)
               (Point (* x n) (* y n))))]
    (test-true "x and y are functions" (and (procedure? x) (procedure? y)))
    (test-equal? "x and y are accessors"
                 (let ([p (Point 2 3)])
                   (list (x p) (y p)))
                 (list 2 3))))


#| Task 2: Simulating attributes |#

#|
(my-class-getter <Class> (<attr> ...)
  (method (<method-name> <param> ...) <body>) ...)

  This macro accepts the *exact* same pattern as my-class from lecture,
  and defines a constructor function that behaves in the same way as well.

  The object returned by the constructor behaves the exact same as the corresponding
  object created by using `my-class`, with one exception. When sent the message
  '__setattr__, the object responds with a method that takes in a symbol <id> and a
  value <v>, and returns a new object that behaves *exactly* the same as the original,
  except when sent the message <id>, it returns <v>.
  This is true regardless of whether <id> was an attribute of the original object.

  Of course, because you may not use mutation, the original object must remain unchanged.

  You may assume that __setattr__ is *not* a field name when using the `my-class-setter`
  macro.
|#
(define-syntax my-class-setter
  (syntax-rules (method)
    [(my-class-setter <Class> (<attr> ...)
       (method (<method-name> <param> ...) <body>) ...)

     ; This is the constructor from lecture. You can (and should) change it here.
     (define (<Class> <attr> ...)
       (lambda (msg)
         (let ([__dict__
                (make-immutable-hash
                 (list (cons (quote <attr>) <attr>)
                       ...
                       (cons (quote <method-name>)
                             (lambda (<param> ...) <body>))
                       ...
                       (cons '__setattr__
                             (lambda (id val)
                               (lambda (msg1)
                                  (let ([__dict1__
                                         (make-immutable-hash
                                          (list (cons (quote <attr>) <attr>)
                                                ...
                                                (cons (quote <method-name>)
                                                      (lambda (<param> ...) <body>))
                                                ...))])
                                    (hash-ref (hash-set __dict1__ id val) msg1
                                              (attribute-error (quote <Class>) msg1))))))

                       ))])

           ; Look up the given attribute in the object's dictionary.
           (hash-ref __dict__ msg
                     ; Raise an error if not attribute not found.
                     (attribute-error (quote <Class>) msg)))))
     ]))


(module+ test
  
  (local
    [(my-class-setter Point (x y)
       (method (size)
               (sqrt (+ (* x x) (* y y))))

       (method (scale n)
               (Point (* x n) (* y n))))]
    (test-true "__setattr__ is a method" (procedure? ((Point 2 3) '__setattr__)))
    (test-equal? "__setattr__ changes an attribute"
                 (let* ([p (Point 2 3)]
                        [p2 ((p '__setattr__) 'x 5)])
                   (p2 'x))
                 5)
    (test-equal? "__setattr__ doesn't mutate original object"
                 (let* ([p (Point 2 3)]
                        [p2 ((p '__setattr__) 'x 5)])
                   (p 'x))
                 2)))
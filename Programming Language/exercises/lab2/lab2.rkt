#lang racket #| CSC324 Winter 2018: Lab 2 |#

#| ★ Task 1: num-pred ★ |#
(define (num-pred pred lst)
  (cond
    [(null? lst) 0]
    [else
     (let ([first-element (first lst)]
           [remain-lst (num-pred pred (rest lst))])
     (if (pred first-element) (+ 1 remain-lst)
         (+ 0 remain-lst)))]))
   
(module+ test
  (require rackunit)
  (require "ex1.rkt") ; replace with the relative path to your exericse 1

  (test-equal? "num-pred/num-evens"
               (num-evens (list 1 2 3 4 5))
               (num-pred even? (list 1 2 3 4 5)))

  ; TODO: replace the ellipsis with the appropriate predicate for this test to pass.
  (test-equal? "num-pred/num-many-evens"
               (num-many-evens (list (list 1 2 3 4 5) (list 2 4 6 10)))

               (num-pred (lambda (x) (< 2 (num-evens x)))
                         (list (list 1 2 3 4 5) (list 2 4 6 10)))))


#| ★ Task 2: make-counter ★ |#
(define (make-counter pred)
  (lambda (x)
    (num-pred pred x)))

; Uncomment and define these using `make-counter`.
(define num-evens-1 (make-counter even?))
(define num-many-evens-1 (make-counter (lambda (x) (< 2 (num-evens-1 x)))))



#| ★ Task 3: Manipulating Racket expressions ★ |#

#|
(extract-ints datum)
  datum: A Racket datum (i.e., quoted expression).

  Returns a list of all *integer* literals appearing in the expression.
  The list should contain the literals in the left-to-right order they appear
  in the text of the quoted expression.

  Use the predicates `list?` and `null?` to check for whether a given expression
  is a list, and an empty list, respectively. You may assume that if the input
  is not a list, then it is an atom (either a symbol or some sort of literal).

  Hint: you may need a recursive helper function to accumulate the results of
  calling extract-ints recursively. Or, lookup the append, map, and append-map
  list functions to see if you get any ideas!

  While you may assume the program is syntactically valid, it is not necessarily
  semantically valid. For example, it might produce type errors at runtime;
  Your function should still correctly return all integer literals appearing in
  the program. See tests for some examples.
|#
(define (extract-ints prog)
  (cond
    [(null? prog) null]
    [(not (list? prog))
     (if (integer? prog) (list prog)
         null)]
    [else
     (let ([first-element (first prog)]
           [rest-elements (extract-ints (rest prog))])
       (cond
         [(list? first-element) (append (extract-ints first-element) rest-elements)]
         [(integer? first-element) (list* first-element rest-elements)]
         [else
          (list* rest-elements)]))]))


(module+ test
  (check-equal? (extract-ints '2) (list 2)) ; Note: '2 == 2, i.e., the ' is unnecessary.
  (check-equal? (extract-ints '(+ 1 2)) (list 1 2))
  (check-equal? (extract-ints '(list 1 2 3 4)) (list 1 2 3 4))
  (check-equal? (extract-ints '(+ (- 9 7) (* 2 8))) (list 9 7 2 8))
  (check-equal? (extract-ints 'ident) (list)))


#|
(replace-324 datum)
  datum: A Racket datum (i.e., quoted expression).

  Return a new datum that is the same as the input, except every occurrence
  of the numeric literal 324 is replaced with 9000.
|#
(define (replace-324 datum)
  (cond
    [(null? datum) null]
    [(not (list? datum))
     (if (equal? 324 datum) 9000
         datum)]
    [else
     (let ([first-element (first datum)]
           [rest-elements (replace-324 (rest datum))])
       (cond
         [(list? first-element) (list* (replace-324 first-element) rest-elements)]
         [(equal? 324 first-element) (list* 9000 rest-elements)]
         [else
          (list* first-element rest-elements)]))]))


(module+ test
  (check-equal? (replace-324 "hello!") "hello!")
  (check-equal? (replace-324 324) 9000)
  (check-equal? (replace-324 '(+ 1 2)) '(+ 1 2))
  (check-equal? (replace-324 '(list 1 2 324 4)) '(list 1 2 9000 4))
  (check-equal? (replace-324 '(+ (- 324 7) (* 2 324))) '(+ (- 9000 7) (* 2 9000))))

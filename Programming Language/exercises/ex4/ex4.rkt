#lang racket #| CSC324 Winter 2018: Exercise 4 |#
#|
★ Before starting, please review the exercise guidelines at
https://www.cs.toronto.edu/~david/csc324/homework.html ★
|#
;-------------------------------------------------------------------------------
(provide matches? my-match)


#|
(matches? pattern value)
  pattern: A datum representing a pattern in the grammar specified in the handout.
  value: An integer, list of integers, or nested list of integers.

  Returns #t if the value matches the given pattern, and #f otherwise.

  You may *not* use built-in Racket pattern-matching in your implementation.

  Note: most Racket higher-order list functions (like map and andmap) can take in more
  than one list.
|#
(define (check-all-true val init)
  (and val init))

(define (matches? pattern expr)
  (cond
    [(and (list? pattern) (equal? (length pattern) 1))
     (matches? (first pattern) expr)]
    [(equal? pattern '_) #t]
    [(equal? pattern '(Plist)) (matches? '() expr)]
    [(not (and (list? pattern) (list? expr))) (equal? pattern expr)]
    [else (let ([first-pattern (first pattern)]
                [rest-pattern (rest pattern)]
                [first-expr (first expr)]
                [rest-expr (rest expr)]
                [pattern-length (length (rest pattern))]
                [expr-length (length expr)])
            (cond
              [(equal? first-pattern 'Plist)
               (if (equal? pattern-length expr-length)
                   (foldl check-all-true #t (map matches? rest-pattern expr))
                   #f)]
              [(equal? first-pattern 'Pcons)
               (if (matches? (first rest-pattern) first-expr)
                   (matches? (rest rest-pattern) rest-expr)
                   #f)
               ]
              [else #f]))]
   ))


(module+ test
  (require rackunit)

  ; Note that we use `test-true` and `test-false` rather than `test-equal?` here.
  (test-true "Simple numeric pattern" (matches? 3 3))
  (test-true "Simple wildcard pattern" (matches? '_ 5))
  (test-true "Simple fixed-length list pattern" (matches? '(Plist 1 _ 3) (list 1 2 3)))
  (test-false "Simple recursive list pattern" (matches? '(Pcons 1 (Plist _ 3)) (list 1 2 4)))
  (test-true "balabal" (matches? '(Plist 1 _ 3 1) (list 1 (list 3 4 5) 3 1)))
  )


#|
(my-match expr pairs)
  expr: An integer, list of integers, or nested list of integers.
  pairs: A datum containing a list of lists, where each inner list has exactly two elements:
         a pattern specified by the grammar from the handout, and an arbitrary value.

  For each pair in `pairs`, this function checks whether the given `expr` matches the pattern,
  and if so, returns the corresponding value.

  Assume that `expr` always matches at least one pattern in `pairs`.
  If the expression matches more than one pattern, return the value corresponding to the
  *first* pattern that the expression matches.

  Again, you may *not* use built-in Racket pattern-matching in your implementation.

  Note: this is very similar to the built-in Racket `match` syntactic form,
  although of course we're only considering a very small subset of patterns here.
|#
(define (my-match expr pairs)
  (let ([first-pairs (first pairs)]
        [rest-pairs (rest pairs)])
    (let([pattern (first first-pairs)]
         [value (rest first-pairs)])
      (if (matches? pattern expr)
          (if (list? value) (first value) value)
          (my-match expr rest-pairs)))))


(module+ test
  (test-equal? "One pair"
               (my-match 4 '((4 "Hello")))
               "Hello")

  (test-equal? "Two pairs"
               (my-match (list 1 2 3) '((4 "Hello")
                                        ((Pcons 1 (Plist _ _)) "Hi")))
               "Hi")
  (test-equal? "mycase1"
               (my-match (list 1 (list 3 4 5) 3 1)
                         '(((Plist 1 _ 3 1) "Bye")))
               "Bye")
  (test-equal? "mycase2"
               (my-match (list 1 (list 3 4 5) 3 1)
                         '(((Plist 1 _ 3 1) "Bye")
                           ((Plist _ _ _ _) "see")))
               "Bye")
  )

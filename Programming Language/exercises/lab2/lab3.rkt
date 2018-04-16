#lang racket #| CSC324 Winter 2018: Lab 2 |#

;-------------------------------------------------------------------------------
; ★ Task 1: Higher-order function practice ★
;-------------------------------------------------------------------------------
#|
(not-pred-1 pred)
  pred: A unary predicate (i.e., a function that takes one argument and returns a boolean).

  Returns a new unary predicate that always returns the negation of what pred returns.
|#
(define (not-pred-1 pred)
  (lambda (x) (not (pred x))))

(module+ test
  (require rackunit)
  
  (test-equal? "even?" ((not-pred-1 even?) 3) #t))


#|
(not-pred-2 pred)

  Same as not-pred-1, except now pred is a *binary* predicate (takes two arguments.
|#
(define (not-pred-2 pred)
  (lambda (x y) (not (pred x y))))

(module+ test
  (test-equal? "string-contains?" ((not-pred-2 string-contains?) "Hello" "ell") #f))

#|
(not-pred pred)

  You can probably guess where this is going. Look up two things:
    - *Rest arguments*, which allow you to define functions that take an
      arbitrary number of arguments.
      <https://docs.racket-lang.org/guide/lambda.html#%28part._rest-args%29>
  
    - The `apply` function.
      <https://docs.racket-lang.org/reference/procedures.html>
 
  Use these to define a generalized `not-pred` that works with predicates of
  arbitrary arity.
|#
(define (not-pred pred)
  (lambda (first-e . rest-e)
    (if (null? rest-e) ((not-pred-1 pred) first-e)
        (apply (not-pred-2 pred) (cons first-e rest-e)))))


(module+ test
  (test-equal? "not-pred/even?" ((not-pred even?) 3) #t)
  (test-equal? "not-pred/string-contains?" ((not-pred string-contains?) "Hello" "ell") #f))


;-------------------------------------------------------------------------------
; ★ Task 2: An arithmetic interpreter ★
;-------------------------------------------------------------------------------

#| Infix Binary Arithmetic Expression Grammar

We define a small grammar as follows:

<expr> = NUMBER | (<expr> <op> <expr>)
<op>   = + | *

NUMBER represents any integer literal in base 10.
Note that this grammar does *not* describe semantically-valid Racket code!
|#

#|
(interpret-arith expr)
  expr: a datum representing a quoted expression generated by the above grammar.

  Returns the value of the given expression (+ and * have their usual mathematical meaning).

  Note: if you want to use pattern matching, you can use structural pattern matching on a list
  of fixed length by using the `list` name in the pattern.

  E.g., the define/match clause [((list a b c)) b] would match '(1 2 3) and evaluate to 2, while the clause
  [((list a 2 c)) c] would match '(1 2 3) but *not* '(2 3 4).
|#
(define (convert-op x a b)
  (cond
   [(equal? x '+) (+ a b)]
   [(equal? x '*) (* a b)]
   [else "Not valid expression"]))

(define (interpret-arith expr)
    (if (not (list? expr)) expr
        (match expr
          [(list a op b) (convert-op op (interpret-arith a) (interpret-arith b))])))

(module+ test
  (test-equal? "Numeric literal" (interpret-arith 3) 3)
  (test-equal? "Simple addition" (interpret-arith '(4 + 5)) 9)
  (test-equal? "Simple multiplication" (interpret-arith '(4 * 5)) 20))


;-------------------------------------------------------------------------------
; ★ Task 3: An arithmetic interpreter with an environment ★
;-------------------------------------------------------------------------------
#| Infix Binary Arithmetic Expression with Identifiers Grammar

We define a small grammar as follows:

<expr> = ID | NUMBER | (<expr> <op> <expr>)
<op>   = + | *

ID represents any valid Racket identifier.
NUMBER represents any integer literal in base 10.
Note that this grammar does *not* describe semantically-valid Racket code!
|#

#|
(interpret-arith-with-ids env expr)
  env: A hash table representing an environment.
       The keys of the hash table are symbols representing identifiers.
  expr: A datum representing a valid expression generated by the above grammar.

  You may assume that all identifiers in `expr` are present in `env`.

  Hint: you should be able to copy-and-paste your implementation of `interpret-arith`
  from above, and just modify it to implement this. Also take a look at the tests.

  Relevant documentation:
    https://docs.racket-lang.org/reference/hashtables.html (lookup `hash-ref`)

  Note: if you want to use define/match here, you need to provide patterns both arguments.
  Since you probably won't need to destructure `env`, you can use a pattern like
  [(env (whatever-pattern-you-want-for-expr)) ...].
|#
(define (dehash env)
  (lambda (x) (hash-ref env x)))

(define (interpret-arith-with-ids env expr)
  (let ([hash-fun (dehash env)]
        [this-fun (lambda (x)
                    (interpret-arith-with-ids env x))])
    (if (not (list? expr))
        (if (number? expr) expr
            (hash-fun expr))
        (match expr
          [(list a op b) (convert-op op (this-fun a) (this-fun b))]))))
    
  


(module+ test
  (let ([env (hash 'a 1)])
    (test-equal? "Simple environment {a: 1}" (interpret-arith-with-ids env '(a + 3)) 4))
  (let ([env (hash 'a 100
                   'b -3)])
    (test-equal? "Simple environment {a: 100, b: -3}" (interpret-arith-with-ids env '(a * b)) -300))
  (let ([env (hash 'a 11
                   'b -11)])
    (test-equal? "complex environment {a: 11, b: -11}" (interpret-arith-with-ids env '((a * b) + (a + b))) -121))
  )
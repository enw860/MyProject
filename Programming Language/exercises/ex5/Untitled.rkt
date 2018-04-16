#lang racket

(define list1 '(1 2 3 4 5))
(define list2 '(2 3 4 5 6))
(define list3 '((1 2 3) (45 10) () (15)))

(define (isElemInlist val acc)
  (let [(isExist (member val acc))]
    (if (equal? isExist #f)
        (append (list val) acc)
        acc)))

(define (remove-dup lst)
  (foldl isElemInlist (list) lst))

(define (union-lst a b)
  (remove-dup (append a b)))

(define (findDiff val acc)
  (let ([isInList (member val acc)])
    (if (equal? isInList #f)
        (append (list val) acc)
        (remove val acc))))

(define (diff val acc)
  (let ([isInList (member val acc)])
    (if (equal? isInList #f)
        acc
        (remove val acc))))

(define (xor-lst a b)
  (foldl findDiff (remove-dup a) (remove-dup b)))

(define (and-lst a b)
  (foldl diff (union-lst a b) (xor-lst a b)))

(define (lst-max ll)
  (map (lambda (x)
         (if (null? x) 0
             (apply max x))) ll))

(define (max-val ll)
  (apply max (lst-max ll)))

(define (max-tup ll)
  (let* ([max-value (max-val ll)]
        [maxIndex (index-of (lst-max ll) max-value)])
    (list-ref ll maxIndex)))

(union-lst  list1 list2)
(xor-lst list1 list2)
(and-lst list1 list2)
(max-val list3)
(max-tup list3)


(define-syntax list-comp
  (syntax-rules (for in)
    [(list-comp <expr>) <expr>]
    [(list-comp <expr> for <var> in <lst>)
     (map (lambda (<var>) <expr>) <lst>)]))


(list-comp (+ 2 3))

(define-syntax my-mac
  (syntax-rules ()
    [(my-mac x) (list x x)]))

(my-mac 3)
(my-mac (+ 3 1))
(my-mac (my-mac 3))

























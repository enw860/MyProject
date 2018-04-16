#lang racket

(define (compile-sub expr)
  (match expr
    [(list '+ a ...) (apply + (map compile-sub a))]
    [(list a) a]
    [_ expr]))

(define (compress-expr expr init end)
  (cond
    [(> init end) (compile-sub expr)]
    [(= init end) expr]
    [else
     (let ([current-expr (list-ref expr init)])
       (if (list? current-expr)
           (compress-expr
            (list-set expr init (compile-sub current-expr))
            (+ 1 init) end)
           (compress-expr expr (+ 1 init) end)))]))

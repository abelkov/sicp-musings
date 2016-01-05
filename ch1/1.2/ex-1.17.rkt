#lang racket

(define (mult a b)
  (cond [(= b 0) 0]
        [(even? b) (mult (double a) (halve b))]
        [else (+ a (mult a (- b 1)))]))

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(mult 0 0)
(mult 0 1)
(mult 1 0)
(mult 1 1)
(mult 1 2)
(mult 2 2)
(mult 5 5)
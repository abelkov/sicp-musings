#lang racket

(define (mult a b)
  (mult-iter a b 0))

(define (mult-iter a b result)
  (cond [(= b 0) result]
        [(even? b) (mult-iter (double a) (halve b) result)]
        [else (mult-iter a (- b 1) (+ a result))]))

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(mult 0 0)
(mult 0 1)
(mult 1 0)
(mult 1 1)
(mult 1 2)
(mult 2 2)
(mult 5 5)
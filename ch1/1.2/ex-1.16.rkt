#lang racket

(define (expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond [(= n 0) a]
        [(even? n) (fast-expt-iter (square b) (/ n 2) a)]
        [else (fast-expt-iter b (- n 1) (* b a))]))

(define (square x) (* x x))

(expt 2 0)
(expt 2 1)
(expt 2 2)
(expt 2 3)
(expt 2 4)
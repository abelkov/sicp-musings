#lang racket

(define (square x) (* x x))

(define (repeated f n)
  (define (iter count x)
    (if (= count 1)
        (f x)
        (f (iter (- count 1)
                 x))))
  (lambda (x) (iter n x)))

((repeated square 2) 5)
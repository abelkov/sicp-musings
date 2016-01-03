#lang racket

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (between? 0.999
            (/ guess (improve guess x))
            1.001))

(define (between? left x right)
  (and (> x left) (< x right)))

(define (square x) (* x x))

(sqrt 1e+75)
(sqrt 0.000000000001)

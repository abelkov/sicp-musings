#lang racket

(define (cube-root x)
  (cbrt-iter 1.0 x))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x)))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (good-enough? guess x)
  (between? 0.999999
            (/ guess (improve guess x))
            1.000001))

(define (between? left x right)
  (and (> x left) (< x right)))

(define (square x) (* x x))

(cube-root 27)

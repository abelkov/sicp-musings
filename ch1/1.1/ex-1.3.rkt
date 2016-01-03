#lang racket

(define (square x) (* x x))

(define (larger-sum-of-squares x y z)
  (cond [(and (> x z) (> y z)) (+ (square x) (square y))]
        [(and (> x y) (> z y)) (+ (square x) (square z))]
        [(and (> y x) (> z x)) (+ (square y) (square z))]))

(larger-sum-of-squares 1 2 3)
(larger-sum-of-squares 2 3 1)
(larger-sum-of-squares 3 1 2)
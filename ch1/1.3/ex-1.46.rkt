#lang racket

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess)
               x))
       0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- (f guess) guess))
       0.0001))
  (define (improve guess)
    (f guess))
  ((iterative-improve good-enough? improve) first-guess))
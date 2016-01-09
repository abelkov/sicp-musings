#lang racket

(define dx 0.001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (repeated f n)
  (define (iter count x)
    (if (= count 1)
        (f x)
        (f (iter (- count 1)
                 x))))
  (lambda (x) (iter n x)))

(define (square x) (* x x))

((repeated (smooth square) 2) 3)
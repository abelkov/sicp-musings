#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (displayln next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (power-transformation x)
  (/ (log 1000)
     (log x)))

(define expon (fixed-point power-transformation 1.5))
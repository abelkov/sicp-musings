#lang racket

(define (cont-frac n d k)
  (define (cont i)
    (if (= i k)
        (/ (n i)
           (d i))
        (/ (n i)
           (+ (d i)
              (cont (+ i 1))))))
  (cont 1))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (* x x))))
  
  (define (d i)
    (- (* 2.0 i)
       1))
  (cont-frac n d k))

(tan 10)
(tan-cf 10 100)
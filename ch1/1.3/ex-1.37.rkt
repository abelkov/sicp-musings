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

(define (cont-frac-iter n d k)
  (define (iter count so-far)
    (if (= 0 count)
        so-far
        (iter (- count 1)
              (/ (n count)
                 (+ (d count)
                    so-far)))))
  (iter k 0))

(define phi 1.6180327868852458)

(* (cont-frac
    (lambda (i) 1.0)
    (lambda (i) 1.0)
    5)
   phi)

(* (cont-frac-iter
    (lambda (i) 1.0)
    (lambda (i) 1.0)
    10)
   phi)

; k = 10 is enough for 4 decimal places of accuracy

(* (cont-frac-iter
    (lambda (i) 1.0)
    (lambda (i) 1.0)
    20)
   phi)
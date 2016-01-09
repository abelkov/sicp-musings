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

(define (d i)
  (if (= (remainder i 3) 2)
      (+ 2.0
         (* 2
            (floor (/ i 3))))
      1))

(define e
  (+ 2
     (cont-frac
      (lambda (i) 1)
      d
      200)))

e

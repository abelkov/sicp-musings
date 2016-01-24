#lang racket

(define (split op1 op2)
  (define (concrete-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (concrete-split painter
                                       (- n 1))))
          (op1 painter
               (op2 smaller smaller)))))
  concrete-split)

(define right-split (split beside below))
(define up-split (split below beside))
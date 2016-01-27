#lang racket

(define (adjoin-set x set)
  (cond [(or (null? set) (< x (car set)))
         (cons x set)]
        [(equal? x (car set)) set]
        [else
         (cons (car set)
               (adjoin-set x (cdr set)))]))

(adjoin-set 1 '(2 3))
(adjoin-set 1 '(1 2 3))
(adjoin-set 4 '(1 2 3))

#lang racket

(define (divides? a b)
  (= (remainder b a) 0))

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car num)
  (if (divides? 2 num)
      (+ 1 (car (/ num 2)))
      0))

(define (cdr num)
  (if (divides? 3 num)
      (+ 1 (cdr (/ num 3)))
      0))

(car (cons 2 3))
(car (cons 5 2))

(cdr (cons 2 3))
(cdr (cons 5 2))

(divides? 3 5)
(divides? 3 6)
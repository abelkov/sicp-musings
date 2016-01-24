#lang racket

(define (square x) (* x x))

(define (square-list1 items)
  (if (null? items)
      null
      (cons (square (car items))
            (square-list1(cdr items)))))

(define (square-list2 items)
  (map square items))

(square-list1 (list 1 2 3 4))
(square-list2 (list 1 2 3 4))
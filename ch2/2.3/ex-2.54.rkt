#lang racket

(define (equal? l1 l2)
  (cond [(and (symbol? l1) (symbol? l2))
         (eq? l1 l2)]
        [(or (symbol? l1) (symbol? l2))
         false]
        [(and (null? l1) (null? l2))
         true]
        [(or (null? l1) (null? l2))
         false]
        [else
         (and (equal? (car l1) (car l2))
              (equal? (cdr l1) (cdr l2)))]))

(equal? '(this is a list) 
        '(this is a list))

(equal? '(this is a list) 
        '(this (is a) list))
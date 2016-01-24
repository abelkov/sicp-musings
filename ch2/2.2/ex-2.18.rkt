#lang racket

(define (reverse l)
  (if (null? l)
      null
      (append (reverse (cdr l))
              (cons (car l) null))))

(reverse (list 1 4 9 16 25))
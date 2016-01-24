#lang racket

(define (same-parity first . rest)
  (define (select l parity)
    (cond [(null? l)
           null]
          [(= (remainder (car l) 2) parity)
           (cons (car l)
                 (select (cdr l) parity))]
          [else (select (cdr l) parity)]))
  (cons first (select rest
                      (remainder first 2))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
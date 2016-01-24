#lang racket

(define x 
  (list (list 1 2) (list 3 (list 4 5 6))))

(define (reverse l)
  (if (null? l)
      null
      (append (reverse (cdr l))
              (cons (car l)
                    null))))

(define (deep-reverse l)
  (cond [(null? l) null]
        [(not (pair? l)) l]
        [else (append (deep-reverse (cdr l))
                 (cons (deep-reverse (car l))
                       null))]))

x
(reverse x)
(deep-reverse x)
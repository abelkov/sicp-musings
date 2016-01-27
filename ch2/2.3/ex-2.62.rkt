#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond [(or (null? set) (< x (car set)))
         (cons x set)]
        [(equal? x (car set)) set]
        [else
         (cons (car set)
               (adjoin-set x (cdr set)))]))

(define (union-set set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [else
         (let ((v1 (car set1))
               (v2 (car set2)))
           (cond [(< v1 v2)
                  (cons v1
                        (union-set (cdr set1) set2))]
                 [(= v1 v2)
                  (cons v1
                        (union-set (cdr set1) (cdr set2)))]
                 [(cons v2
                        (union-set set1 (cdr set2)))]))]))

(union-set '(1 2 3) '(3 4))

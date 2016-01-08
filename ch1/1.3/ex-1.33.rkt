#lang racket
(require math/number-theory)

(define (square x) (* x x))

(define (inc n) (+ n 1))

(define (identity x) x)

(define (filtered-accumulate pred? combiner null-value term a next b)
  (cond
    [(> a b) null-value]
    [(pred? a)
     (combiner (term a)
               (filtered-accumulate
                pred? combiner null-value term (next a) next b))]
    [else
     (filtered-accumulate
      pred? combiner null-value term (next a) next b)]))

(define (sum-squares-prime a b)
  (filtered-accumulate prime? + 0 square a inc b))

(define (product-relatively-prime n)
  (define (rel-prime? i)
    (= 1 (gcd i n)))
  (filtered-accumulate rel-prime? * 1 identity 1 inc (- n 1)))

(sum-squares-prime 1 5)
(product-relatively-prime 4)
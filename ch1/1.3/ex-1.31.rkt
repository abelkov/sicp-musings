#lang racket

(define (square x) (* x x))

(define (inc n) (+ n 1))

(define (identity x) x)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result
                          (term a)))))
  (iter a 1))

(define (factorial n)
  (product-iter identity 1 inc n))

(define (pi n)
  (define (pi-term k)
    (/ (* k (+ k 2))
       (square (+ k 1))))
  (define (pi-next k)
    (+ k 2))
  (* 4.0
     (product pi-term 2 pi-next n)))

#lang racket

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (simpson f a b n)
  (define h (/ (- b a)
               n))
  (define (coeff k)
    (cond [(or (= k 0) (= k n)) 1]
          [(odd? k) 4]
          [else 2]))
  (define (term k)
    (* (coeff k)
       (f (+ a (* k h)))))
  (* (/ h 3.0)
     (sum term 0 inc n)))

(simpson cube 0 1 100)
(simpson cube 0 1 1000)

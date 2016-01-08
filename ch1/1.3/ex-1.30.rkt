#lang racket

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result
                          (term a)))))
  (iter a 0))

(define (cube x) (* x x x))

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
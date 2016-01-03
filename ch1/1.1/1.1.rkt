#lang racket

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (abs x)
  (cond [(> x 0) x]
        [(= x 0) 0]
        [(< x 0) (- x)]))

; 1.1.7 Example: Square Roots by Newton’s Method

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;(sqrt 9)
;(sqrt (+ (sqrt 2) (sqrt 3)))
;(square (sqrt 1000))

; Internal definitions and block structure

;(define (sqrt x)
;  (define (good-enough? guess)
;    (< (abs (- (square guess) x)) 0.001))
;  (define (improve guess)
;    (average guess (/ x guess)))
;  (define (sqrt-iter guess)
;    (if (good-enough? guess)
;        guess
;        (sqrt-iter (improve guess))))
;  (sqrt-iter 1.0))

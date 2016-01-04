#lang racket

; 1.2.1 Linear Recursion and Iteration

(define (factorial n)
  (if (= n 1) 
      1 
      (* n (factorial (- n 1)))))

;(define (factorial n) 
;  (fact-iter 1 1 n))
;
;(define (fact-iter product counter max-count)
;  (if (> counter max-count)
;      product
;      (fact-iter (* counter product)
;                 (+ counter 1)
;                 max-count)))

; 1.2.2 Tree Recursion

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;(define (fib n) 
;  (fib-iter 1 0 n))
;
;(define (fib-iter a b count)
;  (if (= count 0)
;      b
;      (fib-iter (+ a b) a (- count 1))))


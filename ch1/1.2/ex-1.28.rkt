#lang racket

(define (square x) (* x x))

(define (nontrivial-sqrt a n)
  (cond [(= a 1) a]
        [(= a (- n 1)) a]
        [(= 1
            (remainder (square a)
                       n))
         0]
        [else a]))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (nontrivial-sqrt
                   (expmod base (/ exp 2) m)
                   m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 10))

(prime? 561)
(prime? 561)
(prime? 561)
(prime? 561)
(prime? 561)
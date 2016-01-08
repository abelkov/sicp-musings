#lang racket

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (if (= a n)
        #t
        (and (= (expmod a n n) a)
             (try-it (+ 1 a)))))
  (try-it 1))

;> (fermat-test 561)
;#t
;> (fermat-test 1105)
;#t
;> 
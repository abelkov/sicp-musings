#lang racket

(define (new-if predicate 
                then-clause 
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

; The problem with this is we're using applicative-order evaluation,
; so new-if has to evaluate all its arguments to proceed.
; This causes an infinite loop in sqrt-iter.
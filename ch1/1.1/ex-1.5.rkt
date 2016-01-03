#lang racket

(define (p) (p))

(define (test x y) 
  (if (= x 0) 
      0 
      y))

; (test 0 (p))
; returns 0 with normal-order and chokes with applicative-order
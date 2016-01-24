#lang racket

(define (make-segment start-vector end-vector)
  (cons start-vector end-vector))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))
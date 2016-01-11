#lang racket

; church stuff

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (add a b)
  (lambda (f) (lambda (x) ((a f)
                           ((b f) x)))))

; regular stuff for testing

(define (inc x) (+ x 1))

(define (inspect num)
  ((num inc) 0))

(inspect zero)
(inspect one)
(inspect two)

(inspect (add zero one))
(inspect (add one two))
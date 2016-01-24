#lang racket

(define painter-cross
  (segments->painter
   (list (make-segment (make-vector 0 0)
                       (make-vector 1 1))
         (make-segment (make-vector 0 1)
                       (make-vector 1 0)))))
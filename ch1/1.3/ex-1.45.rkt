#lang racket

(define (repeated f n)
  (define (iter count x)
    (if (= count 1)
        (f x)
        (f (iter (- count 1)
                 x))))
  (lambda (x) (iter n x)))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

(define (average x y) (/ (+ x y) 2))

(define (nth-root n x)
  (fixed-point
   ((repeated average-damp (- n 1))
    (lambda (y) (/ x (expt y
                           (- n 1)))))
   1.0))

(nth-root 2 4)
(nth-root 3 8)
(nth-root 4 16)
(nth-root 5 32)
(nth-root 6 64)
(nth-root 7 128)
(nth-root 8 256)
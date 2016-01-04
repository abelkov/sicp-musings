#lang racket

(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

(define (f-iterative n)
  (define (iter a b c count)
    (cond
      [(= count 0) a]      
      [(= count 1) b]
      [(= count 2) c]
      [else (iter b
                  c
                  (+ c (* 2 b) (* 3 a))
                  (- count 1))]))
  (iter 0 1 2 n))

(= (f-recursive 10) (f-iterative 10))

#lang racket

(define (pascal row col)
  (if (or (= col 1) (= col row))
      1
      (+ (pascal (- row 1) (- col 1))
         (pascal (- row 1) col))))

(= (pascal 1 1) 1)
(= (pascal 3 2) 2)
(= (pascal 4 2) 3)
(= (pascal 5 3) 6)

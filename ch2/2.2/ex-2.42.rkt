#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (list-of-zeroes n)
  (if (= n 0)
      null
      (cons 0 (list-of-zeroes (- n 1)))))

(define (replace index lst value)
  (define (replace-iter rest count)
    (cond [(null? rest) null]
          [(eq? count index)
           (cons value (cdr rest))]
          [else
           (cons (car rest)
                 (replace-iter (cdr rest)
                               (+ 1 count)))]))
  (replace-iter lst 1))

(define (unique? val lst)
  (= 1
     (length (filter (lambda (elem) (= val elem))
                     lst))))

(define (queens board-size)
  (define empty-board (list-of-zeroes board-size))
  (define (adjoin-position row col positions)
    (replace col positions row))
  (define (safe-horizontal? col positions)
    (unique? (list-ref positions
                       (- col 1))
             positions))
  (define (safe-diagonal? col positions)
    (define (safe-diag-iter? current)
      (let ((row-current (list-ref positions
                                   (- current 1)))
            (row-last (list-ref positions
                                (- col 1)))
            (diff (- col current)))
        (cond [(= current col) true]
              [(or
                ; \ unsafe
                (and (= col
                        (+ current diff))
                     (= row-last
                        (+ row-current diff)))
                ; / unsafe
                (and (= col
                        (+ current diff))
                     (= row-last
                        (- row-current diff))))
               false]
              [else
               (safe-diag-iter? (+ 1 current))])))
    (safe-diag-iter? 1))
  (define (safe? col positions)
    (and (safe-horizontal? col positions)
         (safe-diagonal? col positions)))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)
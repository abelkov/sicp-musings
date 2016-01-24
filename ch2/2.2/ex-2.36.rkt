#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (firsts seqs)
  (map (lambda (seq) (car seq))
       seqs))

(define (rests seqs)
  (map (lambda (seq) (cdr seq))
       seqs))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (firsts seqs))
            (accumulate-n op init (rests seqs)))))

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(accumulate-n + 0 s)
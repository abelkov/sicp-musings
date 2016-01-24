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

(define (make-triple-sum triple)
  (list (car triple) 
        (cadr triple)
        (caddr triple)
        (+ (car triple) (cadr triple) (caddr triple))))

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap
      (lambda (j) 
        (map (lambda (k)
               (list i j k))
             (enumerate-interval
              1
              (- j 1))))
      (enumerate-interval 
       1 
       (- i 1))))
     (enumerate-interval 1 n)))

(define (sum? triple s)
  (= s (+ (car triple)
          (cadr triple)
          (caddr triple))))

(define (sum-triples n s)
  (map make-triple-sum
       (filter
        (lambda (triple) (sum? triple s))
        (unique-triples n))))

(sum-triples 6 9)

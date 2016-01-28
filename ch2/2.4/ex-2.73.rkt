#lang racket

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (install-deriv-package)
  ;; internal procedures
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) 
           (+ a1 a2))
          (else (list '+ a1 a2))))
  
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  
  (define (addend s) (car s))
  
  (define (augend s) (cadr s))
  
  (define (diff-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) 
               (=number? m2 0)) 
           0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) 
           (* m1 m2))
          (else (list '* m1 m2))))
  
  (define (multiplier p) (car p))
  
  (define (multiplicand p) (cadr p))

  (define (diff-product operands var)
    (make-sum
     (make-product 
      (multiplier operands)
      (deriv (multiplicand operands) var))
     (make-product 
      (deriv (multiplier operands) var)
      (multiplicand operands))))

  (define (make-exponentiation base exp)
    (cond [(= 0 exp) 1]
          [(= 1 exp) base]
          [else (list '** base exp)]))
  
  (define (base exp) (car exp))
  
  (define (exponent exp) (cadr exp))
  
  (define (diff-exponentiation operands var)
    (make-product
     (make-product
      (exponent operands)
      (make-exponentiation
       (base operands)
       (- (exponent operands) 1)))
     (deriv (base operands) var)))

  ;; interface to the rest of the system
  (put 'deriv '+ diff-sum)
  (put 'deriv '* diff-product)
  (put 'deriv '** diff-exponentiation)
  'done)

; 4. we would need to exchange the arguments to `put` above
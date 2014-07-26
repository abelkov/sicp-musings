#lang racket

(define (reverse l)
  (define (rev remaining so-far)
    (if (null? remaining)
        so-far
        (rev (cdr remaining) (cons (car remaining) so-far))))
  (rev l null))

(define (Eval exp env)
  (cond [(Self-evaluating? exp) exp]
        ((Variable? exp) 
         (lookup-variable-value exp env))
        ((Quoted? exp) 
         (Quoted-text exp))
        ((Assignment? exp) 
         (eval-assignment exp env))
        ((Definition? exp) 
         (eval-definition exp env))
        ((If? exp) 
         (eval-if exp env))
        ((Lambda? exp)
         (make-procedure 
          (Lambda-parameters exp)
          (Lambda-body exp)
          env))
        ((Begin? exp)
         (eval-sequence 
          (Begin-actions exp) 
          env))
        ((Cond? exp) 
         (Eval (cond->if exp) env))
        ((Application? exp)
         (Apply (Eval (Application-operator exp) env)
                (list-of-values 
                 (Application-operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: Eval" exp))))

(define (Apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure 
          procedure 
          arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (Procedure-body procedure)
           (extend-environment
             (Procedure-parameters 
              procedure)
             arguments
             (Procedure-environment 
              procedure))))
        (else
         (error "Unknown procedure 
                 type: Apply" 
                procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      null
      (cons (Eval (first-operand exps) env)
            (list-of-values 
             (rest-operands exps) 
             env))))

(define (eval-if exp env)
  (if (true? (Eval (If-predicate exp) env))
      (Eval (If-consequent exp) env)
      (Eval (If-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (Eval (first-exp exps) env))
        (else 
         (Eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) 
                        env))))

(define (eval-assignment exp env)
  (set-variable-value! 
   (Assignment-variable exp)
   (eval (Assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable! 
    (Definition-variable exp)
    (Eval (Definition-value exp) env)
    env)
  'ok)

; ex 4.1

(define (list-of-values-left exps env)
  (if (no-operands? exps)
      null
      (let ([first (Eval (first-operand exps) env)])
        (cons first
              (list-of-values 
               (rest-operands exps) 
               env)))))

(define (list-of-values-right exps env)
  (if (no-operands? exps)
      null
      (let ([rest (list-of-values-right (rest-operands exps) env)])
        (cons (Eval (first-operand exps) env)
              rest))))
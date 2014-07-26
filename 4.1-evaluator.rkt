#lang racket


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
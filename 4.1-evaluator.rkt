#lang racket


(define (evalS exp env)
  (cond [(self-evaluatingS? exp) exp]
        ((variableS? exp) 
         (lookup-variable-value exp env))
        ((quotedS? exp) 
         (quotedS-text exp))
        ((assignmentS? exp) 
         (eval-assignment exp env))
        ((definitionS? exp) 
         (eval-definition exp env))
        ((ifS? exp) 
         (eval-if exp env))
        ((lambdaS? exp)
         (make-procedure 
          (lambdaS-parameters exp)
          (lambdaS-body exp)
          env))
        ((beginS? exp)
         (eval-sequence 
          (beginS-actions exp) 
          env))
        ((condS? exp) 
         (evalS (cond->if exp) env))
        ((applicationS? exp)
         (applyS (evalS (applicationS-operator exp) env)
                (list-of-values 
                 (applicationS-operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: evalS" exp))))
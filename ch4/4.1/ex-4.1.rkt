#lang racket

; ex 4.1

(define (list-of-values-left exps env)
  (if (no-operands? exps)
      null
      (let ([first (Eval (first-operand exps) env)])
        (cons first
              (list-of-values (rest-operands exps) env)))))

(define (list-of-values-right exps env)
  (if (no-operands? exps)
      null
      (let ([rest (list-of-values-right (rest-operands exps) env)])
        (cons (Eval (first-operand exps) env)
              rest))))
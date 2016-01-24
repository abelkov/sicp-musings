#lang racket

(define (for-each operation things)
  (cond [(null? things) 'done]
        [else (operation (car things))
              (for-each operation (cdr things))]))

(for-each 
 (lambda (x) (newline) (display x))
 (list 57 321 88))
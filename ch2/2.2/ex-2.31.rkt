#lang racket

(define (square x) (* x x))

(define (tree-map op tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map op sub-tree)
             (op sub-tree)))
       tree))

(define (square-tree tree) 
  (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
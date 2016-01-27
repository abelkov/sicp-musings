#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (intersection-set set1 set2)
  (define (intersection-ordered set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2)
                 (cons x1 (intersection-ordered 
                           (cdr set1)
                           (cdr set2))))
                ((< x1 x2) (intersection-ordered 
                            (cdr set1) 
                            set2))
                ((< x2 x1) (intersection-ordered
                            set1 
                            (cdr set2)))))))
  (list->tree (intersection-ordered
               (tree->list set1)
               (tree->list set2))))

(define (union-set set1 set2)
  (define (union-ordered set1 set2)
    (cond [(null? set1) set2]
          [(null? set2) set1]
          [else
           (let ((v1 (car set1))
                 (v2 (car set2)))
             (cond [(< v1 v2)
                    (cons v1
                          (union-ordered (cdr set1) set2))]
                   [(= v1 v2)
                    (cons v1
                          (union-ordered (cdr set1) (cdr set2)))]
                   [(cons v2
                          (union-ordered set1 (cdr set2)))]))]))
  (list->tree (union-ordered
               (tree->list set1)
               (tree->list set2))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

(define t1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(define t2
  (make-tree 4
             (make-tree 2 '() '())
             (make-tree 8
                        (make-tree 6 '() '())
                        (make-tree 10
                                   '()
                                   (make-tree 12 '() '())))))

(tree->list (union-set t1 t2))
(tree->list (intersection-set t1 t2))
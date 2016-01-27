#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (encode message tree)
  (define (encode-symbol sym tree)
    (if (leaf? tree)
        '()
        (let ((left (left-branch tree))
              (right (right-branch tree)))
          (cond
            [(member sym (symbols left))
             (cons 0
                   (encode-symbol sym left))]
            [(member sym (symbols right))
             (cons 1
                   (encode-symbol sym right))]
            [else
             (error "unknown symbol: ENCODE-SYMBOL" sym)]))))
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (define (successive-merge set)
    (if (= 1 (length set))
        (car set)
        (let ((merged-tree
               (make-code-tree (car set) (cadr set))))
          (successive-merge
           (adjoin-set merged-tree (cddr set))))))
  (successive-merge (make-leaf-set pairs)))

(define alphabet '((a 2) (na 16) (boom 1) (Sha 3)
                   (Get 2) (yip 9) (job 2) (Wah 1)))

(define message
  '(Get a job
Sha na na na na na na na na

Get a job
Sha na na na na na na na na

Wah yip yip yip yip 
yip yip yip yip yip
Sha boom))

(define tree (generate-huffman-tree alphabet))
(length (encode message tree)) ; 84
(length message) ; 36
; best fixed-length bit number: 3*36 = 108

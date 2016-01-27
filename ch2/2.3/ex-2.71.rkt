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
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

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

(define 5-alphabet '((a 1) (b 2) (c 4) (d 8) (e 16)))
(define 10-alphabet '((a 1) (b 2) (c 4) (d 8) (e 16)
                     (f 32) (g 64) (h 128) (i 256) (j 512)))

(define 5-tree (generate-huffman-tree 5-alphabet))
(define 10-tree (generate-huffman-tree 10-alphabet))

(encode-symbol 'e 5-tree)
(encode-symbol 'a 5-tree)

(encode-symbol 'j 10-tree)
(encode-symbol 'a 10-tree)

; most frequent: 1 bit
; least frequent: n - 1 bits
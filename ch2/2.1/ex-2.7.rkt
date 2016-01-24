#lang racket

; 2.1.4 Extended Exercise: Interval Arithmetic

; Ex. 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (print-interval x)
  (display "[")
  (display (lower-bound x))
  (display ", ")
  (display (upper-bound x))
  (displayln "]"))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

; Ex. 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) 
                    (upper-bound y))
                 (- (upper-bound x) 
                    (lower-bound y))))
;(define (sub-interval x y)
;  (let ((p1 (- (lower-bound x) 
;               (lower-bound y)))
;        (p2 (- (lower-bound x) 
;               (upper-bound y)))
;        (p3 (- (upper-bound x) 
;               (lower-bound y)))
;        (p4 (- (upper-bound x) 
;               (upper-bound y))))
;    (make-interval (min p1 p2 p3 p4)
;                   (max p1 p2 p3 p4))))

; Ex. 2.11
; ?
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Ex. 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (width i)
  (/ (- (upper-bound i) 
        (lower-bound i)) 
     2))

(define (make-center-percent c p)
  (make-center-width c (* p c)))

(define (percent i)
  (/ (width i)
     (center i)))

;let w1 be interval of i1, w2 be interval of i2,
;W be interval of I = i1 + i2, a be lower bound of i1,
;b be upper bound of i1, c be lower bound of i2,
;d be upper bound of i2. Then W = (/ 2 (- (+ b d) (+ a c))) =
;(+ (/ (- b a) 2) (/ (- d c) 2)) = w1 + w2

; for division: w1 of i1 = 1, w2 of i2 = 1.5, W of I = 0.2~

; Ex. 2.10
(define (div-interval x y)
  (if (> (width y) 0)
      (mul-interval x 
                    (make-interval 
                     (/ 1.0 (upper-bound y)) 
                     (/ 1.0 (lower-bound y))))
      (error "interval `y` spans zero")))

(define i1 (make-interval 1 3))
(define i2 (make-interval 5 8))

(print-interval i1)
(print-interval i2)
(newline)

(print-interval (add-interval i1 i2))
(print-interval (sub-interval i2 i1))
(print-interval (mul-interval i1 i2))
(print-interval (div-interval i1 i2))
(newline)

; errors out:
;(div-interval i1
;              (make-interval 6 6))

(displayln (width i1))
(displayln (width (add-interval i1 i2)))
(displayln (width (sub-interval i2 i1)))
(newline)

; Ex. 2.13
(define i3 (make-center-percent 5 0.2))
(define i4 (make-center-percent 3 0.1))
(define i5 (mul-interval i3 i4))

(displayln (percent i3))
(displayln (percent i4))
(displayln (percent i5))
; percent(i5) ~= (percent i3) + (percent i4)

; Ex. 2.14
(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))

(center (div-interval i3 i3))
(percent (div-interval i3 i3))
(center (div-interval i3 i4))
(percent (div-interval i3 i4))

(print-interval (par1 i3 i4))
(print-interval (par2 i3 i4))
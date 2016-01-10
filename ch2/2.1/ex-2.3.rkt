#lang racket

; point

(define (average x y) (/ (+ x y) 2))

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

; find the distance between two points along x or y coordinate
(define (distance p1 p2 coord)
  (if (equal? coord 'x)
      (abs (- (x-point p1)
              (x-point p2)))
      (abs (- (y-point p1)
              (y-point p2)))))

; rectangle representation 1

;(define (make-rectangle tl tr br bl)
;  (list tl tr br bl))

(define (top-left-rectangle rect)
  (car rect))
(define (top-right-rectangle rect)
  (cadr rect))
(define (bottom-right-rectangle rect)
  (caddr rect))
(define (bottom-left-rectangle rect)
  (cadddr rect))

; rectangle representation 2

; Only changes constructor, selectors stay the same.
; I could make the constructor store top-left, width and height
; in a list and change selectors to construct points every time
; but I don't think it would be a good design.
(define (make-rectangle top-left width height)
  (list top-left
        (make-point (+ (x-point top-left) width)
                    (y-point top-left))
        (make-point (+ (x-point top-left) width)
                    (+ (y-point top-left) height))
        (make-point (x-point top-left)
                    (+ (y-point top-left) height))))

; rectangle operations

(define (a-side rect)
  (distance (top-left-rectangle rect)
            (top-right-rectangle rect)
            'x))

(define (b-side rect)
  (distance (top-left-rectangle rect)
            (bottom-left-rectangle rect)
            'y))

(define (perimeter rect)
  (* 2 (+ (a-side rect)
          (b-side rect))))

(define (area rect)
  (* (a-side rect)
     (b-side rect)))

; interactions

;(define r (make-rectangle
;           (make-point 0 0)
;           (make-point 10 0)
;           (make-point 10 5)
;           (make-point 0 5)))

(define r (make-rectangle
           (make-point 0 0)
           10
           5))

(perimeter r)
(area r)
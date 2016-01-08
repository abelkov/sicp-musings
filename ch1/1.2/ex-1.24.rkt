#lang racket

; prime?

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 3))

; timed-prime

(define (search-for-primes start end)
  (cond
    [(> start end) 'done]
    [(even? start)
     (search-for-primes (+ start 1) end)]
    [else
     (timed-prime-test start)
     (search-for-primes( + start 1) end)]))

(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n
                    (- (current-inexact-milliseconds) 
                       start-time))
      (display "")))

(define (report-prime prime elapsed-time)
  (display prime)
  (display " *** ")
  (displayln elapsed-time))

;results:
;
;1000003 *** 0.01611328125
;1000033 *** 0.016845703125
;1000037 *** 0.026123046875
;
;10000019 *** 0.01708984375
;10000079 *** 0.018798828125
;10000103 *** 0.017822265625
;
;100000007 *** 0.02099609375
;100000037 *** 0.02099609375
;100000039 *** 0.026123046875
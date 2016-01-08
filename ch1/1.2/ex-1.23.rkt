#lang racket

; prime?

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else
         (find-divisor n (next test-divisor)))))

(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

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
;100003 *** 0.0380859375
;100019 *** 0.037109375
;100043 *** 0.025146484375
;
;1000003 *** 0.0791015625
;1000033 *** 0.0791015625
;1000037 *** 0.0859375
;
;10000019 *** 0.240966796875
;10000079 *** 0.237060546875
;10000103 *** 0.237060546875
;
;100000007 *** 0.77490234375
;100000037 *** 0.7470703125
;100000039 *** 0.8349609375
;...
;100000000000031 *** 840.846923828125
;100000000000067 *** 825.7548828125
;100000000000097 *** 822.48388671875

; so it's about 1.5x improvement
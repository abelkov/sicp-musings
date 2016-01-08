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
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

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
;100003 *** 0.041015625
;100019 *** 0.0380859375
;100043 *** 0.0390625
;
;1000003 *** 0.1220703125
;1000033 *** 0.117919921875
;1000037 *** 0.1220703125
;
;10000019 *** 0.375
;10000079 *** 0.370849609375
;10000103 *** 0.420166015625
;
;100000007 *** 1.1689453125
;100000037 *** 1.197998046875
;100000039 *** 1.179931640625
;...
;100000000000031 *** 1232.3759765625
;100000000000067 *** 1307.242919921875
;100000000000097 *** 1255.326171875
#lang racket

; prime?

(define (square x) (* x x))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 
         1)
        ((even? n) 
         (square (fast-expt b (/ n 2))))
        (else 
         (* b (fast-expt b (- n 1))))))


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

; we're exponentiating huge bases to huge powers, so it won't work
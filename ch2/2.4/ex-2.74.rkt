#lang racket

; dummy get and put
(define (get op type) true)
(define (put op type) true)

; a file is structured like this:
; ({department-name} {set of records})

(define (get-record employee file)
  (let ((get-record-dept
         (get 'record (department file))))
    (get-record-dept employee file)))

(define (department thing) (car thing))

; a record is structured like this:
; ({department-name} {set of employee info})

(define (get-salary record)
  (let ((get-salary-dept
         (get 'salary (department record))))
    (get-salary-dept record)))

(define (find-employee-record employee files)
  (if (null? files)
      false
      (let ((found (get-record employee (car files))))
        (or found
            (find-employee-record employee (cdr files))))))

; example department package
(define (install-department-1)
  ; the way of getting a record specific to department 1
  (define (get-record employee file) true)
  
  (define (get-salary record) true)
  
  (put 'record 'dept1 get-record)
  (put 'salary 'dept1 get-salary))


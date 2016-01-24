#lang racket

(define (make-mobile left right)
  (cons left right))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))

(define (make-branch length structure)
  (cons length structure))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))

(define (branch-weight branch)
    (let ((structure (branch-structure branch)))
      (if (number? structure)
          structure
          (total-weight structure))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (balanced? mobile)
  (if (number? mobile)
      true
      (let ((left (left-branch mobile))
            (right (right-branch mobile)))
        (and (= (torque left)
                (torque right))
             (balanced? (branch-structure left))
             (balanced? (branch-structure right))))))

(define m (make-mobile (make-branch 5 8)
                       (make-branch 6
                                    (make-mobile (make-branch 2 4)
                                                 (make-branch 2 3)))))

(define b (make-mobile (make-branch 5 8)
                       (make-branch 4
                                    (make-mobile (make-branch 2 5)
                                                 (make-branch 2 5)))))

(total-weight m)
(balanced? m)
(balanced? b)
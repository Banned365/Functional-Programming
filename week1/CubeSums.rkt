#lang racket
(define (cube x)
  (* x x x))

(define (cube-sum x y)
  (+ (cube x) (cube y)))


(define (cube-sums? n)
  (define (cube-iter a b)
    (cond [(= n (cube-sum a b)) #t]
          [(= a b) (cube-iter 1 (add1 b))]
          [(< n (cube-sum a b)) #f]
          [else (cube-iter (add1 a) b)]))
  (cube-iter 1 1))
    

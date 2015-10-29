#lang racket

(define (fib n)
  (define (fib-iter a b current)
    (if (= n current) b
        (fib-iter b (+ a b) (add1 current)))
     )
    (fib-iter 1 1 2))

(define (lucas n)
  (define (lucas-iter a b current)
    (if (= n current) b
        (lucas-iter b (+ a b) (add1 current)))
     )
    (lucas-iter 2 1 2))

(define (summed-memmber n)
  (if (<= n 1) 3
  (+ (fib n) (lucas n))))

#lang racket

(define (fib n)
  (define (fib-iter a b current)
    (cond
      [(= n current) b]
      [(= n 1) 1]
      [else (fib-iter b (+ a b) (add1 current))]
      )
     )
    (fib-iter 1 1 2))

(define (lucas n)
  (define (lucas-iter a b current)
    (cond [(= n current) b]
          [(= n 1) 2]
       [else (lucas-iter b (+ a b) (add1 current))])
     )
    (lucas-iter 2 1 2))



(define (lucas-fib-diff n)
  (if (<= n 1) 1
      (- (lucas n) (fib n))))





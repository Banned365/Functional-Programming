#lang racket
(define (series a b n)
  (define (series-iter a b current)
    (if (= n current) b
        (series-iter b (+ a b) (add1 current)))
     
     )
    (series-iter a b 2))

#lang racket
;Dont know which modules to include in this task...
;

(define (nth-beast-number n)
  (define (beast-iter count res)
    (cond [(= n count) res]
          [else (beast-iter (add1 count) (string-append "666" res))]))
  (beast-iter 1 "666"))

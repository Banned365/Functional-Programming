#lang racket

(require "PalindromeScoreFunctions.rkt")

(define (p-score n)
  (define (p-score-iter res x)
    (cond [(palindrome? x) res]
          [else (p-score-iter (add1 res) (+ x (reverse-int x)))]))
  (p-score-iter 1 n))

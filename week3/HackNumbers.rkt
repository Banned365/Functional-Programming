#lang racket

(require "HackFunctions.rkt")


(define (next-hack n)
  (define (next-hack-iter a)
    (cond [(and (palindrome? (to-binary-string a)) (odd? (occuraences 1 (to-binary-string a)))) a]
          [else (next-hack-iter (add1 a))]))
  (next-hack-iter (add1 n)))

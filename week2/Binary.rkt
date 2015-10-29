#lang racket
(define (string-reverse s)
  (list->string (reverse (string->list s))))

(define (to-binary-string n)
  (cond [(zero? n) 0]
        [else (+ (* 10 (to-binary-string (quotient n 2))) (remainder n 2))]))


(define (binary-to-string str)
  (string->number (number->string str) 2))

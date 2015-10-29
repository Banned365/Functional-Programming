#lang racket
(provide palindrome?)
(provide reverse-int)

(define (palindrome? n)
  (if (= n (reverse-int n)) #t
      #f))

(define (reverse-int n)
  (define (rev-iter n result)
    (if (= n 0) result
        (rev-iter (quotient n 10) (+ (* result 10) (remainder n 10))))
  )
  (rev-iter n 0)
)

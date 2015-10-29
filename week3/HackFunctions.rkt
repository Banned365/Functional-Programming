#lang racket
(provide reverse-int)
(provide palindrome?)
(provide occuraences)
(provide to-binary-string)


(define (reverse-int n)
  (define (rev-iter n result)
    (if (= n 0) result
        (rev-iter (quotient n 10) (+ (* result 10) (remainder n 10))))
  )
  (rev-iter n 0)
)

(define (palindrome? n)
  (if (= n (reverse-int n)) #t
      #f))

(define (occuraences a x) 
  (define (occurances-iter current result x)
    (cond [(= x 0) result]
          [(= a (remainder x 10)) (occurances-iter (add1 current) (add1 result) (quotient x 10))]
          [else (occurances-iter (add1 current) result (quotient x 10))]))
  (occurances-iter 0 0 x))


(define (to-binary-string n)
  (cond [(zero? n) 0]
        [else (+ (* 10 (to-binary-string (quotient n 2))) (remainder n 2))]))

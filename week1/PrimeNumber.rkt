#lang racket
(define (non-divisible-by i m)
  (cond
    [(= i 1) true]
    [else (cond
            [(= (remainder m i) 0) false]
            [else (non-divisible-by (- i 1) m)])]))

(define (prime? p)
  (define (non-divisible-by n d)
    (cond
     ((= d 1) #t)
     (else (if(= (remainder n d) 0)
          #f
          (non-divisible-by n (- d 1))))))
  (if (= p 1)
      #t
      (non-divisible-by p (- p 1))))

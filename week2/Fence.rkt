#lang racket
(define (string-repeat str n)
  (cond 
    [(< n 1) ""]
    [(= n 1) str]
    [else    (string-append str (string-repeat str (sub1 n)))]))



(define (fence n)
  (string-append (string-append (string-append (string-append (string-append (string-append "{" (string-repeat "-" (round (+ 1 (log n))))) ">") (~a n)) "<") (string-repeat "-" (round (+ 1 (log n))))) "}"))

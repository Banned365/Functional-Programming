#lang racket


(define (simplify-frac frac)
  (let ([g (gcd (car frac) (cdr frac))])
    (cons (/ (car frac) g) (/ (cdr frac) g))))


(define (add-frac frac1 frac2)
  (simplify-frac (cons (+ (* (car frac1) (cdr frac2)) (* (cdr frac1) (car frac2))) (* (cdr frac1) (cdr frac2)))))


(define (substract-frac frac1 frac2)
  (simplify-frac (cons (- (* (car frac1) (cdr frac2)) (* (cdr frac1) (car frac2))) (* (cdr frac1) (cdr frac2)))))

(define (mult-frac frac1 frac2)
  (simplify-frac (cons (* (car frac1) (car frac2)) (* (cdr frac1) (cdr frac2)))))

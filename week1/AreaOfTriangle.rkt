#lang racket
(define (lenght a b c)
  (* 1/2 (+ (+ a b) c )))

(define (area a b c)
  (sqrt (* (* (* (lenght a b c) (- (lenght a b c) a)) (- (lenght a b c) b)) (- (lenght a b c) c))))


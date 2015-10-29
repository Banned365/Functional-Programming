#lang racket
(define (square x)
  (* x x))


(define (square-difference x y)
  (- (square x) (square y)))

(define (circle? circle-x circle-y radius point-x point-y)
  (<= (+ (square-difference point-x circle-x) (square-difference point-y circle-y)) (square radius)))

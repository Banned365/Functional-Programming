#lang racket
(define (tree-level n tree)
   (cond
    [(empty-tree? tree) '()]
    [(= n 1) (list(node tree))]
    [else (append (tree-level (- n 1) (left tree))
                  (tree-level (- n 1) (right tree)))]))

(define (tree-levels tree)
  (define (tree-iter n l res)
    (cond
      [(= n 0) res]
      [else (tree-iter (- n 1) l (cons (tree-level n xs) res))]))
  (tree-iter (height tree) tree '()))

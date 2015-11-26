#lang racket
(define (tree-map f tree)
  (cond
    [(empty-tree? tree) '()]
    [else (make-tree (f (node tree)) (tree-map f (left tree))
                                     (tree-map f (right tree)))]))

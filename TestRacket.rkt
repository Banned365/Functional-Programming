#lang racket

(define (sum-div x)
  (define (helper count res)
    (cond [(= count x) res]
          [(zero? (remainder x count))
                  (helper (add1 count) (+ res count))]
          [else (helper (add1 count) res)]))
  (helper 1 0))

(define (sum-interesting k)
  (define (interesting? x)
    (= x (sum-div (sum-div x))))
  (cond [(= k 1) 0]
        [(interesting? k) (+ k (sum-interesting (- k 1)))]
        [else (sum-interesting (- k 1))]))

(define (zero matrix)
  (define (contains-zero? lst)
    (cond [(empty? lst) #f ]
          [(= (first lst) 0) #t]
          [else (contains-zero? (rest lst))]))
  (define (make-zeros lst)
    (if (empty? lst) '() (cons 0 (make-zeros (rest lst)))))
  (define (helper res matr)
    (cond [(empty? matr) res]
          [(contains-zero? (first matr))
           (helper (append res (list (make-zeros (first matr)))) (rest matr))]
          [else (helper (append res (list (first matr))) (rest matr))]))
  (helper '() matrix))

(define (sum-digits n)
  (if (= n 0) 0 (+ (remainder n 10) (sum-digits (quotient n 10)))))

(define (digits-sum n)
  (define (compose k res)
    (if (= k 1) res (compose (- k 1) (+ 1 (* res 10)))))
  (define limit (compose n 1))
  (define (contains-zero? n)
    (cond [(< n 10) #f]
          [(zero? (remainder n 10)) #t]
          [else (contains-zero? (quotient n 10))]))
  (define (helper count res)
    (cond [(> count limit) res]
          [(and (not (contains-zero? count))
                (= (sum-digits count) n)) (helper (add1 count) (+ res count))]
          [else (helper (add1 count) res)]))
  (helper 0 0))

(define (all-permutations? items)
  (define (number->list n)
    (if (< n 10) (list n) (append (number->list (quotient n 10)) (list (remainder n 10)))))
  (define (permutation? l1 l2)
    (cond [(empty? l2) #t]
          [(not (member (first l2) l1)) #f]
          [else (permutation? l1 (rest l2))]))
  (cond [(= 1 (length items)) #t]
        [(not (permutation? (number->list (first items)) (number->list (second items)))) #f]
        [else (all-permutations? (rest items))]))

(define (rotate-once lst)
  (cons (last lst) (take lst (- (length lst) 1))))

(define (find-pos lst n)
  (define (helper count l)
    (cond [(empty? l) -1]
          [(equal? n (first l)) count]
          [else (helper (add1 count) (rest l))]))
  (helper 0 lst))

(define (rotate lst times)
  (if (= times 0) lst (rotate (rotate-once lst) (- times 1))))

(define (cycle times items)
  (lambda (n) (find-pos (rotate items times) n)))

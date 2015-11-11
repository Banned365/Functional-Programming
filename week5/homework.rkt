#lang racket

(define (reverse-int n)
  (define (rev-iter n result)
    (if (= n 0) result
        (rev-iter (quotient n 10) (+ (* result 10) (remainder n 10))))
  )
  (rev-iter n 0)
)


; Намира сумата на всички числа в numbers
; -> (sum (list))
; 0
; -> (sum (list 1 2 3))
; 6
(define (sum numbers)
  (foldr + 0 numbers))

; Проверява дали x се среща в items
; -> (member? 1 (list 1 2 3))
; #t
; -> (member? "asdf" (list "asd"))
; #f
; Разгледайте http://docs.racket-lang.org/reference/booleans.html
(define (member? x items)
  (cond [(empty? items) #f]
        [(equal? x (car items)) #t]
        [else (member? x (cdr items))]))

; -> (length2 (range2 1 10))
; 9
; В Racket има такава функция, наречена length
(define (length2 items)
  (define (lenght-iter counter items)
    (cond [(empty? items) counter]
          [else (lenght-iter (add1 counter) (cdr items))]))
  (lenght-iter 0 items))

; Връща n-тия елемент от items при 0лево базиран индекс
; -> (list-ref2 (list 1 2 3) 0)
; 1
; В Racket има такава функция, наречена list-ref
(define (list-ref2 items n)
  (define (iter counter items)
    (cond [(equal? n counter) (car items)]
          [else (iter (add1 counter) (cdr items))]))
  (iter 0 items))

; -> (range2 1 10)
; '(1 2 3 4 5 6 7 8 9)
; В Racket съществува такава функция, наречена range
(define (range2 a b)
  (cond [(> a b) (list)]
        [else (cons a (range2 (add1 a) b))]))

; Строи списък от числата между 0 и n, включително, като прилага f върху всяко число
; i-тия елемент на този списък е (f i)
; -> (build-list2 3 id)
; '(0 1 2)
; -> (build-list2 3 (lambda (x) (* x x)))
; '(0 1 4)
; В Racket има такава функция, наречена build-list

(define (build-list2 n f)
  (define (iter counter listt)
    (cond [(equal? counter n) (reverse listt)]
          [else (iter (add1 counter) (cons (f counter) listt))]))
  (iter 0 (list)))

; Конкатенира два списъка в нов списък
; -> (append2 (list 1 2 3) (list 4 5 6))
; '(1 2 3 4 5 6)
; В Racket има такава фунцкия, наречена append
(define (append2 l1 l2)
  (cond [(empty? l1) l2]
        [else (cons (car l1) (append2 (cdr l1) l2))]))

; Обръща списъка наобратно
; -> (reverse2 (list 1 2 3))
; '(3 2 1)
; В Racket има такава функция, наречена reverse
(define (reverse2 items)
  (define (iter itemss res)
    (cond [(empty? itemss) res]
          [else (iter (cdr itemss) (cons (car itemss) res))]))
  (iter items (list)))

; Взима първите n елемента от даден списък
; Ако (> n (length items)), тогава връща items
; -> (take2 3 (list 1 2 3 4 5))
; '(1 2 3)
(define (take2 n items)
  (cond [(zero? n) (list)]
        [else (cons (car items) (take2 (sub1 n) (cdr items)))]))

; Маха първите n елемента от даден списък
; Ако (> n (length items)) връща '()
; -> (drop2 3 (list 1 2 3 4 5))
; '(4 5)
(define (drop2 n items)
  (cond [(zero? n) items]
        [drop2 (sub1) (cdr items)]))

; Функция от по-висок ред. Взима поредни елементи от items докато предиката p за тях дава истина
; -> (take-while zero? (list 0 0 0 1 2 3))
; '(0 0 0)
; -> (take-while even? (list 2 4 5 7 8 3 2))
; '(2 4)
; -> (take-while (lambda (x) (> x 3)) (list 1 1 1 1 1))
; '()
(define (take-while p items)
  (if (not (p (car items))) (list)
      (cons (car items) (take-while p (cdr items)))))

; Функция от по-висок ред. Маха поредните елементи от items докато предикатa p дава лъжа за тях
; -> (drop-while zero? (list 0 0 0 1 2 3))
; '(1 2 3)
; -> (drop-while even? (list 2 4 5 7 8 3 2))
; '(5 7 8 3 2)
; -> (drop-while (lambda (x) (> x 3)) (list 1 1 1 1 1))
; '(1 1 1 1 1)
(define (drop-while p items)
  (cond [(not (p (car items))) items]
        [drop-while p (cdr items)]))

; Функцията взима число и връща списък от цифрите му
; -> (number->list 123)
; '(1 2 3)

(define (number->list n)
  (define (iter n)
   (cond [(< n 10) (list n)] 
        [else (cons (remainder n 10) (iter (quotient n 10)))]))
  (reverse (iter n)))


; Функцията взима списък от цифри и връща числото
; -> (list->number (list 1 2 3))
; 123
(define (list->number ns)
  (define (iter listt res)
    (cond [(empty? listt) res]
          [(iter (cdr listt)(+ ( * res 10) (car listt)))]))
  (iter ns 0))

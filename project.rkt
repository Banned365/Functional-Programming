#lang racket
 
(require unstable/list)

;;Константа за размера на дъската ( 3x3 )
(define STANDART-SIZE 3)

;; Построяване на игровата дъска
(define (new-board size)
  (define (new-row) (build-list size (lambda (l) " ")))
  (build-list size (lambda (l) (new-row))))

;;Взима елемента на позиция y-ред и x-колона
(define (board-at board x y)
  (list-ref (list-ref board y) x))

;;Предикат който прави проверка дали дадена позиция е свободна за да се сложи даден символ
(define (is-free? board x y)
  (string=? (board-at board x y) " "))

;;Предикат който пвари провекра дали на дадена позиция сомволът е еднакъв с "c"
(define (is-marked? board c x y)
  (string=? (board-at board x y) c))

;; Функция която маркира дадена позиция на дъската с символа "c"( "Х" или "О" )
(define (mark board c x y)
  (cond
    ((or (out-of-bounds? board x y) (not (is-free? board x y)) ) board)
    (else (list-update board y (lambda (l) (list-set l x c))))))

;;Предикат който проверява дали въведените координати не са извън границата на игровата дъска
(define (out-of-bounds? board x y)
  (let ((size (length board)))
    (or (>= x size) (< x 0) (>= y size) (< y 0))))

;;Предикат който проверява за равенство
(define (is-draw? board)
  (zero? (count (lambda (s)
                  (string=? s " ")) (flatten board))))

;;Предикат който проверява за победа
(define (is-win? board c)
  (or (check-horiz? board c) (check-vert? board c) (check-diag? board c)))

;;Предикатите който се ползват в по-горния предикат ( Те правят проверка по редове,колони и диагонали)
(define (check-horiz? board c)
  (let ((size (length board)))
    (< (count false?
              (map (lambda (y) (has-filled-row? board c y))
                   (range size))) size)))
 
(define (has-filled-row? board c y)
  (= (count (lambda (l)
              (string=? l c)) (list-ref board y)) (length board)))
 
(define (check-vert? board c)
  (let ((size (length board)))
    (< (count false?
              (map (lambda (x) (has-filled-column? board c x))
                   (range size))) size)))
 
(define (has-filled-column? board c x)
  (let ((size (length board)))
    (= (count (lambda (b) (not (false? b)))
              (map (lambda (l)
                     (string=? (list-ref (list-ref board l) x) c))
                   (range size))) size)))
 
(define (check-diag? board c)
  (let ((size (length board)))
    (define (check-secondary-diag?)
      (zero? 
       (count false?
              (map (lambda (n)
                     (is-marked? board c n (- size 1 n))) (range size)))))
    (define (check-main-diag?)
      (zero?
       (count false?
              (map (lambda (n)
                     (is-marked? board c n n)) (range size)))))
    (or (check-secondary-diag?) (check-main-diag?))))

;;Преобразува листа от листове в годен вид :D
(define (board->string board)
  (define (row->string row)
    (string-join
     (map (lambda (x) (string-append "(" x ")")) row) ""))
  (string-join
   (map (lambda (x)
          (row->string (list-ref board x)))
        (range (length board))) "\n"))

;;Сменя символа на играча който е наред
(define (next-player current-player)
  (cond
    ((string=? current-player "X") "O")
    (else "X")))

;; Input прозорчето с помоща на което въвеждаме координатите на клетката която искаме да сложим нашия сомвол
(define (enter-int s)
  (display s)
  (read))

;; Предикат който проверява дали играта е свършила (Ако да , автоматично приключва играта )
(define (check-status board c)
  (cond ((is-win? board c)
         (displayln (string-append "Играч | " c " | победи !! Браво , за награда получавате усмивка в конзолата!"))
         (displayln ":)")
         (displayln (board->string board)) (exit))
        ((is-draw? board)
         (displayln "Равенство! Можете и по-добре... Вярвам във вас !!!") (exit))))

;; Главната функция от която стартираме играта
(define (tic-tac-toe-start size)
  (displayln "        ========== Мроски шах ============ ")
  (displayln "За играта са нужни двама играчи, които се редуват в слагането на символи на полето.")
  (displayln "Целта на играта е да се подредят еднакви символи било то в ред,колона или диагонал за да победи даден играч.")
  (displayln "Координатите на играта започва от 0")
  (displayln "Х-координатата определя колоната ,а О-координатата определя реда( (1 1) е в центъра при 3х3 дъска )")
  (tic-tac-toe-loop (new-board size) "X"))

;; Функцията която "върти" играта докато някой играч не победи,загуби или има равенство
(define (tic-tac-toe-loop board p)
  (displayln (board->string board))
  (displayln (string-append "Ред е на | " p " | играч."))
  (let ((new-board (tic-tac-toe-input board p)))
    (cond
      ((is-draw? board) (display "Равенство!") (exit))
      (else (tic-tac-toe-loop new-board (next-player p))))))
 
(define (tic-tac-toe-input board p)
  (let ((x (enter-int "Въведете X-координата: "))
        (y (enter-int "Въведете Y-координата: ")))
    (cond
      ((are-numbers? x y)
       (displayln "Невалиден вход. Моля използвайте цифри ,а не букви....")
       (tic-tac-toe-input board p))
      (else
       (let ((new-board (mark board p x y)))
         (cond
           ((not (equal? board new-board))
            (displayln (string-append "Вие маркирахте ("
                                      (number->string x) ", "
                                      (number->string y) ")"))
            (check-status new-board p) new-board)
           (else
            (displayln "Това поле вече е маркирано или извън полето.Гледайте по внимателно каде слагате...")
            (tic-tac-toe-input board p))))))))
 
(define (are-numbers? x y)
  (not (and (number? x) (number? y))))

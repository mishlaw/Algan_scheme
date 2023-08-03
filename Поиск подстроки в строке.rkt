#lang scheme
;Метод грубой силы 
(define (Brute-Force sub str)
(define n (string-length sub))
(define rez (memf (λ (i) (equal? sub (substring str i (+ i n)))) (build-list (+ 1 (- (string-length str) n)) values)))

(if (equal? rez #f) #f (car rez)))

;С помощью хеш-функции
(define (str2sum str) ;Считает сумму кодов символов строки
(foldl (λ (x s) (+ s (char->integer x))) 0 (string->list str)))

(define (find-hash sub str) ;Главная функция
(define A (str2sum sub)) ;Сумма кодов символов подстроки
(define len1 (string-length sub)) ;дЛИНА ПОДСТРОКИ
(define len2 (string-length str)) ;длина строки
  
(define (find-iter str B n) ;В-сумма кодов текущей строки, от которой мы отрезаем начало и добавляем конец ( по одной букве)
                            ;n-столько символов мы уже обработали
  (if (= A B) ;Если сумма кодов подстроки равна сумме кодов строки
      (if (equal? sub (substring str 0 len1)) n
          (if (>= n (- len2 len1)) #f
              (find-iter (substring str 1) (+ (- B (char->integer (string-ref str 0)))(char->integer (string-ref str len1)))(+ n 1))))
      (if (>= n (- len2 len1)) #f
          (find-iter (substring str 1) (+ (- B (char->integer (string-ref str 0))) (char->integer (string-ref str len1))) (+ n 1)))))

  (if (> len1 len2) #f (find-iter str (str2sum (substring str 0 len1)) 0)))

;С конечным автоматом
(define (what-num x dic) ;порядковый номер буквы в алфавите
(define tmp (findf (lambda (a) (equal? (car a) x)) dic) )
(if (equal? tmp #f) #f (cdr tmp) ) )

(define (make-dic s) ;Список списков из буквы и порядкового номера в данном алфавите (сюда подается список букв)
(foldl (lambda (x dic) (if (equal? (what-num x dic) #f)
                           (cons (cons x (+ (cdar dic) 1)) dic) dic) ) (list (cons (car s) 0)) s))

(define (change-item lst i item) ;Добавляем элемент на i-тое место в список
(append (take lst i) (cons item (drop lst (+ i 1)))))

(define (iter lines next-num str dic) 
(define n (length lines))
(if (empty? str) lines
    (let* ((i (what-num (car str) dic))
           (next (- n (list-ref (list-ref lines next-num) i))))
      (iter (cons (change-item (list-ref lines next-num) i (+ n 1)) lines) next (cdr str) dic))))

(define (make-auto s dic) ;Сам конечный автомат (список букв подстроки и словарь)
(reverse (iter (list (change-item (build-list (length dic) (lambda (x) 0)) (what-num (car s) dic) 1)) 0 (cdr s) dic)))

(define (search-auto sub str)
(define (search state s)
(if (empty? s) (if (= state n) 
     (- (string-length str) (length s) n) #f)
    (if (= state n) (- (string-length str) (length s) n)
        (let ((num (what-num (car s) dic)))
          (if (equal? num #f) (search 0 (cdr s))
              (search (list-ref (list-ref auto state) num) (cdr s)))))))
(define s (string->list sub))
(define n (length s))
(define dic (make-dic s))
(define auto (make-auto s dic))
(write auto)
(search 0 (string->list str)))
#lang racket
;Сортировка выбором, сложность О(n^2)
(define (find-max lst max res)
    (if (null? lst) (cons max res)
        (if (< (car lst) max)
            (find-max (cdr lst) max (cons (car lst) res))
            (find-max (cdr lst) (car lst) (cons max res)))))
(define (sort-vibor lst)
  (define (iter lst res)
    (if (null? lst) res
        (let ((r (find-max (cdr lst) (car lst) null)))
          (iter (cdr r) (cons (car r) res)))))
  (iter lst null))

;Сортировка выбором, написанная на практике (меньшая сложность (наверное)) (рекурсия)
(define (pick-sort lst)
  (if (null? lst) lst
      (let (( m (push-min lst))) (cons (car m) (pick-sort (remove (car m) lst))))))
(define (push-min lst) ;Вытаскивает минимальный элемент в начало
  (foldl (λ (n res) (if (< n (car res)) (cons n res) (cons (car res) (cons n (cdr res))))) (list (car lst)) (cdr lst)))

;Сортирует по любому критерию (функция f)

(define (sort-vibor1 lst f )
  (define (find-max lst m res)
    (if (null? lst) (cons m res)
        (if (f (car lst) m)
            (find-max (cdr lst) m (cons (car lst) res))
            (find-max (cdr lst) (car lst) (cons m res)))))
  (define (iter lst res)
    (if (null? lst) res
        (let ((r (find-max (cdr lst) (car lst) null)))
          (iter (cdr r) (cons (car r) res)))))
  (iter lst null))


; Сортировка пузырьком!
; 
; Алгоритм многократно бежит по списку и меняет соседей местами в зависимости
; от заданной функции сравнения. Алгоритм заканчивает работу, если всё стоит так,
; как нужно.
; 
; (5 3 1 6 2 8 4)
; Первый цикл: (3 1 5 2 6 4 8)
; Второй цикл: (1 3 2 5 4 6 8) (можно бежать по циклу без учёта последней цифры,
;                                     ведь максимум списка точно там)
; Третий цикл: (1 2 3 4 5 6 8) (можно бежать по циклу без учёта последних двух цифр)
; 
; В базовой версии за цикл алгоритм ставит на нужное место один элемент.
; В версии получше можно вести учет последней замены - ведь если последняя замена была
; раньше конца списка, то все последующие элементы стоят на своих местах. За счёт этого
; алгоритм имеет шанс стать быстрее! Однако, есть минус - алгоритм способен передвинуть
; элемент на любое количество шагов вправо, если необходимо, но влево способен лишь на
; один шаг. Как бороться? Пойдём в обратную сторону! (работаем с тем, что между !)
; (5 3 1 6 2 8 4 9 10) -> (3 1 5 2 6 4 ! 8 9 10) -> (1 ! 3 2 5 4 6 ! 8 9 10) и т.д.
; 
; Сортировка пузырьком бежит по списку за n + (n - 1) + ... + 1 = (n+1)*n*(0.5) = O(n^2).
; В худшем случае вычислительная мощность остается такой же, как и у сортировки выбором,
; но в лучшем случае он сравним с линейным!
; 
; Комментарии по алгоритму в лекции:
; "Функция b-iter на самом деле рекурсия! Возвращение немаксимального элемента происходит
; на обратном ходе рекурсии за счет cons, значит, пользоваться reverse совершенно не нужно.
; Список рано или поздно станет пустым, значит, мы делаем список из максимального элемента и
; обратным ходом постепенно добавляем к нему все локальные максимумы. Эта версия нам не нравится,
; потому что мы дважды бежим по списку и рассматриваем то, что можно вообще не трогать."
; 
; Мы напишем совсем хорошую версию! Когда идём в одну сторону, то ставим элементы по возрастанию,
; а обратно - по убыванию. Будем писать сразу с функцией.
; 
; find-max скопировали из прошлой сортировки, но мы хотим запомнить место последней замены,
; т.е. счётчик k.

;Пузырек (базовая)
(define (bubble lst)
(define (b-iter head tail)
  (if (empty? tail) (list head)
      (if (> head (car tail))
          (cons (car tail) (b-iter head (cdr tail)))
          (cons head (b-iter (car tail) (cdr tail))) ) ) )
  (define (repeat n lst)
    (if (= n 0) lst
        (repeat (- n 1) (b-iter (car lst) (cdr lst))) ) )
  (repeat (- (length lst) 1) lst) )



;Сортировка пузырьком с лекции
(define (sort-bubble lst f)
  (define (not-f x y) (not (f x y)))
  (define (move k lst1 lst2)  ; перенести k элементов из lst1 в lst2
    (if (= k 0)
        (cons lst1 lst2)
        (move (- k 1) (cdr lst1) (cons (car lst1) lst2))
        )
    )
  (define (find-max left_lst m list_rezult k f) ; делаем замены в списке, k - сколько переместить
    (if (null? left_lst)
        (cons k (cons m list_rezult))
        (if (f (car left_lst) m)
            (find-max (cdr left_lst) m (cons (car left_lst) list_rezult) 1 f)
            (find-max (cdr left_lst) (car left_lst) (cons m list_rezult) (+ k 1) f)
            )
        )
    )
  (define (main list_a list_b list_c) ; list_a - то, что не трогаем слева, list_c - справа, list_b - то, с чем работаем
    (if (null? list_b)
        (append (reverse list_a) list_c)  ; не самый худший append, он сработает лишь единожды и за линейное время
        (let* ((rezult_1 (find-max (cdr list_b) (car list_b) '() 1 f))
               (rezult_2 (move (car rezult_1) (cdr rezult_1) list_c)))
          (if (null? (car rezult_2))
              (append (reverse list_a) (cdr rezult_2))
              (let*
                  ((rezult_3 (find-max (cdar rezult_2) (caar rezult_2) '() 1 not-f))
                (rezult_4 (move (car rezult_3) (cdr rezult_3) list_a)))
                (main (cdr rezult_4) (car rezult_4) (cdr rezult_2)))
                )
              )
          )
        )
  (main '() lst '())
  )

;сортировка включением

(define (sort-insert lst)
(cond
  [(empty? lst) empty]
  [(cons? lst) (insert (car lst) (sort-insert (cdr lst)))]))

(define (insert n lst) ;Совершает включение (находит лучшую позицию)
(cond
  [(empty? lst) (list n)]
  [else (cond
        [(<= n (car lst)) (cons n lst)]
        [(> n (car lst)) (cons (car lst) (insert n (cdr lst)))])]))


;быстрая сортировка
; Не лучшая реализация, т.к приходится пробегать 3 раза по списку из-за фильтра.
(define (qu-sort lst f)
  (if (null? lst) lst 
  (append (qu-sort (filter (λ(x) (f x (car lst))) lst) f)
          (filter (λ(x) (equal? x (car lst))) lst)
          (qu-sort (filter (λ(x) (f (car lst) x)) lst) f))))

;Разделение списка на части происходит за один проход (быстрее работает)
(define (Hoar sp)
(if (null? sp)  null
    (append (Hoar (car (razd sp)))   (cadr (razd sp)) (Hoar (cddr (razd sp))))))

(define (razd sp)
  (define (vsp sp first sp< sp= sp>)
    (if (null? sp) (cons sp< (cons sp= sp>))
        (if (< (car sp) first) (vsp (cdr sp) first (cons (car sp)  sp<) sp= sp>)
           (if (= (car sp) first)
                (vsp (cdr sp) first sp< (cons (car sp) sp=) sp>)
            (vsp (cdr sp) first  sp< sp= (cons (car sp) sp>))))))
  (vsp sp (car sp) null null null))


;сортировка слиянием

(define (sort-sl lst f)
  (define (sort-dop lst n)
    (if (<= n 1) lst
        (let ((n2 (quotient n 2)))
        (slij (sort-dop (take lst n2) n2)
              (sort-dop (drop lst n2) (- n n2))))))
  (sort-dop lst (length lst)))

(define (slij a b)
   (if (null? a) b
                       (if (null? b) a
                           (if (< (car a) (car b))
                               (cons (car a) (slij (cdr a) b))
                               (cons (car b ) (slij a (cdr b)))))))

;Итерацией
(define (sort-sl-it lst)
(define (iter lst k n)
; преобразование k-упорядоченного списка в 2k-упорядоченный
(define (2k lst k n)
(if (>= k n) lst
(append (slij (take lst (min k n))
              (take (drop lst (min k n)) (min k (- n k))))
        (2k (drop lst (min (* 2 k) n)) k (max (- n (* 2 k)) 0) ))))
  (if (>= k n) lst
      (iter (2k lst k n) (* k 2) n)))
(iter lst 1 (length lst)))


;Пирамидальная
(define (pir-sort vect)
  (define k (vector-length vect))
  (define k2 (quotient k 2))
  
  (define (swap i j)
    (define x (vector-ref vect i))
    (vector-set! vect i (vector-ref vect j))
    (vector-set! vect j x))
  
  (define (vp i n)
    (cond [(< i (quotient n 2))
           (let ((p (if (or (>= (+ 2 (* i 2)) n)
                            (>= (vector-ref vect (+ 1 (* i 2))) (vector-ref vect (+ 2 (* i 2)))))
                        (+ 1 (* i 2))
                        (+ 2 ( * i 2)))))
             (cond [(> (vector-ref vect p) (vector-ref vect i))
                    (begin (swap i p) (vp p n))]))]))
  
  (for-each (λ (i ) (vp i k)) (build-list k2 (λ (t) (- k2 t 1))))
  (for-each (λ (n) (swap 0 n) (vp 0 n)) (build-list (- k 1) (λ ( t) (- k t 1)))) vect)

;Поразрядная
; Для сортировки строк и натуральных чисел (2 варианта). Если строки: сравниваем сначала последние буквы (справа налево),
; но т.к они разной длины , то где нет буквы окажется в начале , потом те, у которых вторая буква поменьше, и т д,
; на финальном этапе сравниаем первые буквы. Сложность О(nlog10от длины самого большого числа). 19 списков т.к сортиует любые целые
; ( для минуса тоже создаются списки). Для обычных подойдет 10
(define (sort-razr lst)
  (define (get-digit x n) ;Получает n+1 разряд и прибавляет к нему 9
    (+ 9 (remainder (quotient x (expt 10 n)) 10)))

  (define (kol x) ;Колличество разрядов
    (if (= x 0) 0 (+ 1 (kol (quotient x 10)))))

  (define (constr v) ;склеивает списки
    (foldl (λ (n rez) (foldl cons rez (vector-ref v n))) null (build-list 19 (λ(i) (- 18 i)))))

  (define (iter lst v n) ;Раскидывает n-ые разряды по спискам
    (if (null? lst) (constr v)
        (iter (cdr lst) (let ((p (get-digit (car lst) n)))
                          (vector-set! v p (cons (car lst) (vector-ref v p))) v) n) ))

  (foldl (λ (n lst) (iter lst (make-vector 19 null) n)) lst (build-list (kol (apply max lst)) +) ;(0 1 2 3..max-1)
         ))

  
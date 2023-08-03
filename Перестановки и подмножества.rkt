#lang racket
;Перестановки
;Рекурсия. (учитывает повторяющиеся элементы, глупый алгоритм)
(define (Per sp)
 (define (it lst res)
   (if (null? lst) (list res)
   (append-map  (λ (x)  (it (remove x lst) (cons x res))) lst)))
  (it sp null))
;Выводит в файл перестановки (такой же глупый)
(define (ass lst file)
  (define out (open-output-file file #:exists 'replace #:mode 'text))
  (define (help lst1 lst2)
    (if (null? (cdr lst1))
        (writeln (reverse (cons (car lst1) lst2)) out)
        (for-each (λ(x) (help (remove x lst1) (cons x lst2))) lst1))) (help (sort lst <) null) (close-output-port out))

;Умный (исключает повторы)
(define (per-p lst file)
  (define out (open-output-file file #:exists 'replace)) ;mode чтобы нормально пользовать write

  (define (help lst1 lst2)
    
   (define (iter lst)
   (cond [(and (not (empty? (cdr lst))) (not (equal? (car lst) (cadr lst))));Если в списке больше одного элемента и первый не равен 2
             (help (remove (cadr lst) lst1) (cons (cadr lst) lst2))])
      (cond [(not (empty? (cdr lst))) (iter (cdr lst))]))

    (if (null? lst1)  (writeln (reverse lst2) out)
        (iter (cons #f lst1))))
  
  (help (sort lst <) null)
  (close-output-port out))

;Подмножества
(define (Podmn lst) 
  (define (vybor n lst)
    (if (= n 0) null
        (if (odd? n) (cons (car lst) (vybor (quotient n 2) (cdr lst))) (vybor (quotient n 2) (cdr lst)))))
  (for-each (λ(i) (write (vybor i lst))) (range (expt 2 (length lst)))))








  

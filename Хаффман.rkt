#lang scheme
;Дерево по коду"
;Список,первый элемент которого минимальный (он единствеенный)"

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "Wrong value" bit))))

(define ( find-min m lst) ;Отодвигает минимальный вес в конец списка
  (if (null? lst) (list m)
      (if (< (weight m)  (weight (car lst)))
          (cons (car lst) (find-min m (cdr lst)))
          (cons m (find-min (car lst) (cdr lst))))))

(define (Huf-code lst)
  (define (iter trees )
    (if (null? (cdr trees)) (car trees)
        ;попозже напишем:)
        (let* ((m1 (reverse(find-min (car trees) (cdr trees))))
               (m2 (reverse (find-min (cadr m1) (cddr m1)))))
          (iter (cons (make-code-tree (car m1) (car m2))
                      (cdr m2))))))
  (iter (map (λ (p) (make-leaf (car p) (cdr p))) lst)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

; Процедура decode-1 принимает два аргумента: список остающихся битов и текущую позицию в дереве. Она двигается «вниз» по дереву,
; выбирая левую или правую ветвь в зависимости от того, ноль или единица следующий бит в списке
; (этот выбор делается в процедуре choose-branch).Когда она достигает листа, она возвращает символ из него как очередной
; символ сообщения, присоединяя его посредством cons к результату
;  декодирования остатка сообщения, начиная от корня дерева.
;   

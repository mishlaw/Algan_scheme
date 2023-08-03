#lang racket
(define (circle pol)
  (define last (list-ref pol (- (length pol) 1)))
  (if (equal? last (car pol)) pol (append pol (list (car pol)))));замыкает многоугольник

; Особенность обработки многоугольников состоит в том, на очередном
; шаге алгоритма приходится анализировать сторону многоугольника, а это
; две вершины, т.е. два элемента списка, поэтому имеющиеся в scheme
; функции map и foldl для этого не подходят.
; Создадим собственные функции, аналогичные foldl, andmap и map. В
; каждом из этих вариантов арность функции оказывается на 1 больше, чем в
; оригинале.


(define (fold-p func init lst);foldl для многоугольников
  (if (or (empty? lst) (empty? (cdr lst))) init
      (fold-p func (func (car lst) (cadr lst) init) (cdr lst))))

(define (and-p func lst)
  (if (or (empty? lst) (empty? (cdr lst))) #t
      (if (func (car lst) (cadr lst)) (and-p func (cdr lst)) #f)));andmap for polygon

(define (map-p func lst)
  (if (or (empty? lst) (empty? (cdr lst))) '()
      (cons (func (car lst) (cadr lst)) (map-p func (cdr lst)))));the best version of map for polygones

(define (vec t1 t2);make-vector
  (cons (- (car t2) (car t1)) (- (cdr t2) (cdr t1))))

(define (norm v);teorema pifagora
  (sqrt (+ (sqr (car v)) (sqr (cdr v)))))

(define (dist a b);расстояние
  (norm (vec a b)))

; периметр произвольного многоугольника
(define (perimetr pol)
  (fold-p (lambda (a b per) (+ per (dist a b)))
          0 (circle pol)))

;Скалярное произведение
(define (sp a b)
  (+ (* (car a) (car b)) (* (cdr a) (cdr b))))

;Векторное произведение (length of vp)
(define (vp a b)
  (- (* (car a) (cdr b)) (* (cdr a) (car b))))

;Проверка на выпуклость
;1. Работает для мн-ков без самоперсечений
(define (convex? pol)
  (define sides (circle (map-p vec (circle pol))));стороны
  (define z (vp (car sides) (cadr sides))) ;в качестве проверочного надо использовать ненулевое векторное произведение
  (and-p (lambda (a b) (>= (* (vp a b) z) 0)) sides))
;2  точки лежат по одну сторону от прямой.
; Если точки находятся по разные стороны относительно прямой, то косые произведения имеют разные знаки,
; а значит их произведение отрицательно.
; Если же точки лежат по одну сторону относительно прямой,
; то знаки косых произведений совпадают, значит, их произведение положительно.


; Принадлежность точки выпуклому многоугольнику
(define (in-convex? point pol);принадлежность точки сломается если точка лежит на продолжении стоороны
  (define sides (map-p vec (circle pol)));опять надо найти ненулевое z
  (define to-point (map (lambda (t) (vec t point)) pol));векторы до точки от вершин
  (define z (vp (car sides) (car to-point)))
  (andmap (lambda (a b) (>= (* (vp a b) z) 0)) sides to-point))

;Площадь многоугольника
(define (square pol)
(abs (fold-p (λ (a b s) (+ s (* 1/2 (+ (cdr a) (cdr b)) (- (car b) (car a))))) 0 (circle pol))))

;Принадлежность точки многоугольнику (через 8 секторов)
(define (razn x y)
  (define z (- x y))
  (if (> z 4) (- z 8) (if (< z -4) (+ z 8) z)))

(define (point-in-pol point pol)
 (define new-pol (map (λ(t) (vec point t)) (circle pol)))
 (not (= 0 (remainder (foldl (λ (z x sum) ((if (> z 0) + -) sum x)) 0 (map-p vp (circle (map-p vec (circle new-pol))))
  (map-p razn 
  (circle (map (λ(v) (if (> (cdr v) 0)   ;Набор характеристик (каким областям принадлежат вершины)
                 (if (> (car v) 0)
                     (if (> (car v) (cdr v)) 0 1)
                          (if (> (- (car v)) (cdr v)) 3 2))
                 (if (> (car v) 0)  (if (> (car v) (- (cdr v))) 7 6)  (if (> (- (car v)) (- (cdr v))) 4 5))))
   new-pol)))) 16))))

;Триангуляция выпуклого многоугольника
(define (polygon-triangulation polygon)
  (define start-point (car polygon))
  (define (iter current-point next-point polygon triangles)
    (if (empty? (cdr polygon))
       (cons (list start-point current-point next-point) triangles)
       (iter next-point (cadr polygon) (cdr polygon) (cons (list start-point current-point next-point) triangles))))
  (iter (cadr polygon) (caddr polygon) (cddr polygon) '()))

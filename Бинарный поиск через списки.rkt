#lang Scheme
;Первый из искомых элементов (если они идут подряд)
(define (bin-poisk sp x)
  (define (iter l r)
    (define c (quotient (+ l r) 2))
    (if (= l r)
        (if (= (list-ref sp c) x) c false)
        (if (> x (list-ref sp c)) (iter (+ c 1) r) ;Продолжаем в правой
            (iter l c)))) ;В левой
  (iter 0 (- (length sp) 1)))

;Последний (если они идут подряд)
(define (bin-poisk2 sp x)
  (define (iter l r)
    (define c (quotient (+ l r 1) 2))
    (if (= l r)
        (if (= x (list-ref sp c)) l false)
        (if (>= x (list-ref sp c)) (iter c r) (iter l (- c 1)) )))
  (iter 0 (- (length sp) 1)))
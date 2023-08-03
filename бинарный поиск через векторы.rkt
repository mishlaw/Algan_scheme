#lang Scheme
;Ничем не отличается от реализации на списках
"Находит первый x из имеющихся"
(define (bin-search1 v x)
  (define (iter l r)
    (define c (quotient (+ l r) 2))
    (if (= l r)
        (if (= x (vector-ref v l)) l false)
        (if (<= x (vector-ref v  c))
            (iter l c)
            (iter (+ 1 c) r))))
  (iter 0 (- (vector-length v)  1)))

"Возвращает последний из х"
(define (bin-search2 v x)
  (define (iter l r)
    (define c (quotient (+ l r 1) 2))
    (if (= l r)
        (if (= x (vector-ref v l)) l false)
        (if (< x (vector-ref v  c))
            (iter l (- c 1))
            (iter  c r))))
  (iter 0 (- (vector-length v)  1)))
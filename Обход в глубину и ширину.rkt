#lang racket
;используется для поиска циклов в графе
(define (DFS a b G);start final graph
  (define (iter prosm stack);следит за просмотренными вершинами и состояниями стека
    (if (> (list-ref prosm a) 0) stack ;база
        (if (empty? stack) #f ;еще один повод остановиться, если стек пустой, то пути нет
            (let* ((pos (car stack));находим позицию
                   (next (foldl (lambda (x y);x - номер вершины, y - то что копим
                                  (if (equal? y #f)
                                      (if (= 0 (list-ref prosm x)) x #f);еще непросмотренная вершина?
                                      y))
                                #f (list-ref G pos)))
                   (step (list-ref prosm pos)));номер шага
              (if (equal? next #f) (iter prosm (cdr stack)) ;если в тупике, убираем вершину стека
                  (iter (append (take prosm next);есть куда идти, пополняем стек

                                (cons (+ step 1) (drop prosm (+ next 1))))

                        (cons next stack)))))))
  (iter (build-list (length G) (lambda (i) (if (= i b) 1 0))) (list b)));стартуем с точки b



;BFS реализуется на очередях
;двигамся не по одному пути, а сразу по всем, которые есть из данной вершины
;вместо нулевой вершины помещаем в очередь те, кторые выходят из 0 и тд,
;те в данный момент в очереди находятся те вершины, до которых дошли, но от которых никуда не ходили
;пустая очередь - признак посещенной компоненты связности
(define (BFS a b G)
  (define (iter prosm queue)
    (if (> (list-ref prosm a) 0)  (- (list-ref prosm (car queue)) 1) 
        (if (empty? queue) #f
            (let* ((pos (car queue))

                   (next (filter (lambda (x) (= 0 (list-ref prosm x)));список возможных вершин
                                 (list-ref G pos)))
                   (step (list-ref prosm pos)));шаг
              (if (not next) (iter prosm (cdr queue));убираем элемент из очереди
                  (iter (map (lambda (i)
                               (if (= 0 (list-ref prosm i))
                                   (if (equal? (member i next) #f) 0

                                       (+ step 1))
                                   (list-ref prosm i)))
                             (build-list (length G) values))
                        (append (cdr queue) next)))))))
  (iter (build-list (length G) (lambda (i) (if (= i b) 1 0))) (list b)))

;Дополнение к графу
(define (graph-addition adj-lists)
  (define vertexes (build-list (length adj-lists) values))
  (foldr (λ [adj-list vertex res]
           (cons (remove vertex (remove* adj-list vertexes)) res)) '() adj-lists vertexes))



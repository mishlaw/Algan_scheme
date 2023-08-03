#lang racket
;вычисление выраженний в ОПН
(define (calc pol)
  (car (foldl (lambda (x s)
                (cond
                  ((equal? x '+) (cons (+ (cadr s) (car s)) (cddr s)))
                  ((equal? x '-) (cons (- (cadr s) (car s)) (cddr s)))
                  ((equal? x '*) (cons (* (cadr s) (car s)) (cddr s)))
                  ((equal? x '/) (cons (/ (cadr s) (car s)) (cddr s)))
                  ((equal? x '^) (cons (expt (cadr s) (car s)) (cddr s)))
                  (else (cons x s))))
              '() pol)))

(define (calc-2 pol);by Lakhtin
  (define (iter pol stack)
    (if (null? pol) (car stack)
        (cond
          ((or ( equal? (car pol) '+)
               ( equal? (car pol) '-)
               ( equal? (car pol) '*)
               ( equal? (car pol) '/))
           (iter (cdr pol) (cons ((eval (car pol)) (cadr stack) (car stack)) (cddr stack))))
          ((equal? (car pol) '^)
           (iter (cdr pol) (cons (expt (cadr stack) (car stack)) (cddr stack))))
          (( equal? (car pol) '~)
           (iter (cdr pol) (cons (- (car stack)) (cdr stack))))
          (else (iter (cdr pol) (cons (car pol) stack))))))
  (iter pol '()))

(define (make-pol lst);algoritm by Deikstra feat. Lakhtin
  (define (prior op);lada priora
    (cond
      ((equal? op '<) 0)
      ((equal? op '+) 1)
      ((equal? op '-) 1)
      ((equal? op '*) 2)
      ((equal? op '/) 2)
      ((equal? op '^) 3)
      (else -1)))

  (define (do-s lst stack rez) ;Делает выражение внтури скобки
    (if (equal? (car stack) '<)
        (iter (cdr lst) (cdr stack) rez)
        (do-s lst (cdr stack) (cons (car stack) rez))))

  (define (do-p lst stack rez)
    (if (or (null? stack) (< (prior (car stack)) (prior (car lst))))
        (iter (cdr lst) (cons (car lst) stack) rez)
        (do-p lst (cdr stack) (cons (car stack ) rez))))
  
  (define (iter lst stack rez)
    (if (null? lst) (append (reverse rez) stack)
        (let ((p (prior (car lst))))
          (cond (( equal? (car lst) '<) (iter (cdr lst) (cons (car lst) stack) rez))
                (( equal? (car lst) '>) (do-s lst stack rez))   
                (( < p 0) (iter (cdr lst) stack (cons (car lst) rez))) ;Если встретили операнд (цифру)
                (else (do-p  lst stack rez))))))
  (iter lst '() '()))
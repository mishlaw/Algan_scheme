#lang scheme
;Упорядочивает по алфавиту
(define (chast)
  (define in (open-input-file "f1.txt"))
  (define out (open-output-file "f2.txt" #:exists 'replace))
  (define v  (make-vector 1140 0))
  
  (define (iter)
    (define x (read-char in))
      (if (equal? x eof) v
          (let ((code (char->integer x)))
            (vector-set! v code (+ (vector-ref v code) 1))
            (iter))))
  
    (define (print-chast i)
      (if (= i 1140) (close-output-port out)
          (let ((x (integer->char i))
                (val (vector-ref v i)))
            (cond [ (> val 0) (begin (write x out)
                                     (display #\ out)
                                     (display val out)
                                     (display #\return out)
                                     (display #\newline out))])
            (print-chast (+ i 1)))))
  
    (iter)
  (print-chast 0))
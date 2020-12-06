#! /usr/bin/env racket
#lang racket

; lower? -> (or/c list? #f)
(define (lower? code)
  (member code '(F L)))

; upper? -> (or/c list? #f)
(define (upper? code)
  (member code '(B R)))

; (: search (-> Integer Integer (Listof Symbol) Integer))
(define (search lo hi codes)
  (if (null? codes)
      lo
      (let ([range (/ (- hi lo) 2)]
            [c     (car codes)]
            [codes (cdr codes)])
           (cond [(lower? c) (search lo          (- hi range) codes)]
                 [(upper? c) (search (+ lo range) hi          codes)]))))

; (: codes->seat (-> (Listof Symbol) Integer))
(define (codes->seat codes)
  (define/contract row-codes
    (listof (symbols 'F 'B))
    (take codes 7))
  (define/contract col-codes
    (listof (symbols 'L 'R))
    (drop codes 7))
  (define row (search 0 128 row-codes))
  (define col (search 0   8 col-codes))
  (define seat (+ (* 8 row) col))
  seat)

; (: read (-> Path-String (Listof (Listof Symbol))))
(define (read filepath)
  (map (λ (s) (map (λ (c) (string->symbol (string c)))
                   (string->list s)))
       (file->lines filepath)))

; (: main (-> Void))
(define (main)
  (define data (read "input.txt"))
  (displayln (apply max (map codes->seat data)))
  (newline))

(main)

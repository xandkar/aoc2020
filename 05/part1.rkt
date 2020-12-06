#! /usr/bin/env racket
#lang racket

(define/contract (lower? code)
  (-> (symbols 'F 'B 'L 'R)
      (or/c list? #f))
  (member code '(F L)))

(define/contract (upper? code)
  (-> (symbols 'F 'B 'L 'R)
      (or/c list? #f))
  (member code '(B R)))

; (: search (-> Integer Integer (Listof Symbol) Integer))
(define/contract (search lo hi codes)
  (-> natural-number/c
      natural-number/c
      (listof (symbols 'F 'B 'L 'R))
      natural-number/c)
  (if (null? codes)
      lo
      (let ([range (/ (- hi lo) 2)]
            [c     (car codes)]
            [codes (cdr codes)])
           (cond [(lower? c) (search lo           (- hi range) codes)]
                 [(upper? c) (search (+ lo range) hi           codes)]))))

; (: codes->seat (-> (Listof (U 'F 'B 'L 'R)) Integer))
(define/contract (codes->seat codes)
  (-> (listof (symbols 'F 'B 'L 'R))
      natural-number/c)
  (define/contract row-codes (listof (symbols 'F 'B)) (take codes 7))
  (define/contract col-codes (listof (symbols 'L 'R)) (drop codes 7))
  (define row (search 0 128 row-codes))
  (define col (search 0   8 col-codes))
  (define seat (+ (* 8 row) col))
  seat)

; (: read (-> Path-String (Listof (Listof (U 'F 'B 'L 'R)))))
(define/contract (read filepath)
  (-> path-string?
      (listof (listof (symbols 'F 'B 'L 'R))))
  (map (λ (s) (map (λ (c) (string->symbol (string c)))
                   (string->list s)))
       (file->lines filepath)))

; (: main (-> Void))
(define/contract (main)
  (-> void?)
  (define data (read "input.txt"))
  (displayln (apply max (map codes->seat data)))
  (newline))

(main)

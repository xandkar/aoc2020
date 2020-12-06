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

(define/contract (drop-while-adjacent nums)
  (-> (not/c (empty? (listof natural-number/c)))
      (listof natural-number/c))
  (define (drop prev nums)
    (cond [(null? nums)
           '()]
          [(> (abs (- prev (car nums))) 1)
           nums]
          [else
            (drop (car nums) (cdr nums))]))
  (drop (car nums) (cdr nums)))

; (: main (-> Void))
(define/contract (main)
  (-> void?)
  (define occupied (map codes->seat (read "input.txt")))
  (define possible
    (flatten
      (map (lambda (row) (define r (* 8 row)) (map (lambda (c) (+ r c))
                                                   (range 8)))
           (range 128))))
  (define unoccupied
    (set->list (set-subtract (list->set possible) (list->set occupied))))
  (define/contract unoccupied-and-available
    (property/c length (=/c 1))
    (drop-while-adjacent (sort (drop-while-adjacent (sort unoccupied <)) >)))
  (define my-seat (first unoccupied-and-available))
  (printf "part 1: ~a\n" (apply max occupied))
  (printf "part 2: ~a\n" my-seat))

(main)

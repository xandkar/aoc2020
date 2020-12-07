#! /usr/bin/env racket
#lang racket

(define/contract (codes->seat codes)
  (-> (listof (one-of/c #\F #\B #\L #\R))
      natural-number/c)
  (define (code->bin sym)
    (match sym
           [#\F #\0]
           [#\B #\1]
           [#\L #\0]
           [#\R #\1]))
  (string->number (apply string (map code->bin codes)) 2))

(define/contract (read filepath)
  (-> path-string?
      (listof (listof (one-of/c #\F #\B #\L #\R))))
  (map string->list (file->lines filepath)))

(define/contract (drop-while-adjacent nums)
  (-> (not/c (empty? (listof natural-number/c)))
      (listof natural-number/c))
  (define (drop prev nums)
    (cond [(null? nums)                     '()]
          [(> (abs (- prev (car nums))) 1)  nums]
          [else                             (drop (car nums) (cdr nums))]))
  (drop (car nums) (cdr nums)))

; (: main (-> Void))
(define/contract (main)
  (-> void?)
  (define occupied (map codes->seat (read "input.txt")))
  (define possible (range (* 128 8)))
  (define unoccupied
    (set->list (set-subtract (list->set possible) (list->set occupied))))
  (define/contract unoccupied-and-available
    (property/c length (=/c 1))
    (drop-while-adjacent (sort (drop-while-adjacent (sort unoccupied <)) >)))
  (printf "part 1: ~a\n" (apply max occupied))
  (printf "part 2: ~a\n" (first unoccupied-and-available)))

(main)

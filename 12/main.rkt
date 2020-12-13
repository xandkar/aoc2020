#! /usr/bin/env racket
#lang racket

(struct state (f x y) #:transparent)

(define code/c (or/c 'N 'S 'E 'W 'L 'R 'F))
(define instruction/c (cons/c code/c number?))

(define/contract (string->ins s)
  (-> string? instruction/c)
  (match (regexp-match (pregexp "^([A-Z]{1})([0-9]+)$") s)
         [(list _ code magnitude) (cons (string->symbol code)
                                        (string->number magnitude))]))

(define/contract (read filepath)
  (-> path-string? (listof instruction/c))
  (map string->ins (file->lines filepath)))

(define (dir->num d) (match d ['N 0] ['E 1] ['S 2] ['W 3]))
(define (num->dir n) (match n [0 'N] [1 'E] [2 'S] [3 'W]))

(define (turn from in by)
  (define op (match in ['R +] ['L -]))
  (num->dir (modulo (op (dir->num from) (/ by 90)) 4)))

(define (move instruction s)
  (match instruction
         [(cons 'N n) (struct-copy state s [y (+ (state-y s) n)])]
         [(cons 'S n) (struct-copy state s [y (- (state-y s) n)])]
         [(cons 'E n) (struct-copy state s [x (+ (state-x s) n)])]
         [(cons 'W n) (struct-copy state s [x (- (state-x s) n)])]
         [(cons 'L n) (struct-copy state s [f (turn (state-f s) 'L n)])]
         [(cons 'R n) (struct-copy state s [f (turn (state-f s) 'R n)])]
         [(cons 'F n) (match (state-f s)
                             ['N (struct-copy state s [y (+ (state-y s) n)])]
                             ['S (struct-copy state s [y (- (state-y s) n)])]
                             ['E (struct-copy state s [x (+ (state-x s) n)])]
                             ['W (struct-copy state s [x (- (state-x s) n)])])]))

(define (manhattan s)
  (match s [(state _ x y) (+ (abs x) (abs y))]))

(define (main)
  (for-each
    (λ (input-filepath)
       (define data (read input-filepath))
       (define state0 (state 'E 0 0))
       (define state1 (foldl move state0 data))
       (printf "~a part-1 ~a\n" input-filepath (manhattan state1)))
    (list "example.txt"
          "input.txt"
          )))

(main)

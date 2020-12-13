#! /usr/bin/env racket
#lang racket

(define abs-face-dir/c (or/c 'N 'S 'E 'W))
(define rel-face-dir/c (or/c 'L 'R))
(define move-dir/c 'F)
(define code/c (or/c abs-face-dir/c
                     rel-face-dir/c
                     move-dir/c))
(define instruction/c (cons/c code/c number?))
(define state/c (list/c abs-face-dir/c number? number?))

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

(define/contract (turn from in by)
  (-> abs-face-dir/c rel-face-dir/c (or/c 90 180 270) abs-face-dir/c)
  (define op (match in ['R +] ['L -]))
  (num->dir (modulo (op (dir->num from) (/ by 90)) 4)))

(define (get-f s) (list-ref s 0))
(define (get-x s) (list-ref s 1))
(define (get-y s) (list-ref s 2))

(define (set-f s val) (list-set s 0 val))
(define (set-x s val) (list-set s 1 val))
(define (set-y s val) (list-set s 2 val))

(define/contract (move1 instruction s)
  (-> instruction/c state/c state/c)
  (match instruction
         [(cons 'N n) (set-y s (+ (get-y s) n))]
         [(cons 'S n) (set-y s (- (get-y s) n))]
         [(cons 'E n) (set-x s (+ (get-x s) n))]
         [(cons 'W n) (set-x s (- (get-x s) n))]
         [(cons 'L n) (set-f s (turn (get-f s) 'L n))]
         [(cons 'R n) (set-f s (turn (get-f s) 'R n))]
         [(cons 'F n) (match (get-f s)
                             ['N (set-y s (+ (get-y s) n))]
                             ['S (set-y s (- (get-y s) n))]
                             ['E (set-x s (+ (get-x s) n))]
                             ['W (set-x s (- (get-x s) n))])]))

(define (manhattan s)
  (+ (abs (get-x s)) (abs (get-y s))))

(define (main)
  (for-each
    (λ (input-filepath)
       (define data (read input-filepath))
       (printf "~a part-1 ~a\n"
               input-filepath
               (manhattan (foldl move1 '(E 0 0) data))))
    (list "example.txt"
          "input.txt")))

(main)

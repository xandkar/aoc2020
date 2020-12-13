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

(define (turn2 x y by)
  (match
    by
    [(or 90  -270) (cons (- y)    x)]
    [(or 180 -180) (cons (- x) (- y))]
    [(or 270 -90 ) (cons    y  (- x))]))

(define (move2 instruction states)
  (match
    states
    [(cons (cons sx sy) (cons wx wy))
     (match
       instruction
       [(cons 'N n) (cons (cons    sx              sy          ) (cons    wx    (+ wy n)   ))]
       [(cons 'S n) (cons (cons    sx              sy          ) (cons    wx    (- wy n)   ))]
       [(cons 'E n) (cons (cons    sx              sy          ) (cons (+ wx n)    wy      ))]
       [(cons 'W n) (cons (cons    sx              sy          ) (cons (- wx n)    wy      ))]
       [(cons 'L n) (cons (cons    sx              sy          ) (turn2   wx       wy    n ))]
       [(cons 'R n) (cons (cons    sx              sy          ) (turn2   wx       wy (- n)))]
       [(cons 'F n) (cons (cons (+ sx (* wx n)) (+ sy (* wy n))) (cons    wx       wy      ))])]))

(define (manhattan x y)
  (+ (abs x) (abs y)))

(define (main)
  (for-each
    (λ (input-filepath)
       (define data (read input-filepath))
       (define s1 (foldl move1 '(E 0 0) data))
       (pretty-write (list 's1 s1))
       (printf "~a part-1 ~a\n"
               input-filepath
               (manhattan (get-x s1) (get-y s1)))
       (define s2 (car (foldl move2 (cons (cons 0 0) (cons 10 1)) data)))
       (pretty-write (list 's2 s2))
       (printf "~a part-1 ~a\n"
               input-filepath
               (manhattan (car s2) (cdr s2))))
    (list "example.txt"
          "input.txt")))

(main)

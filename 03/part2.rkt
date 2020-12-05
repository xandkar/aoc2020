#!/usr/bin/env racket
#lang racket

(define (ref data r k)
  (vector-ref (vector-ref data r) k))

(define (walk d h w r k ri ki f state)
  (if (< r h)
      (let ([state (f (ref d r k) state)]
            [r     (+ r ri)]
            [k     (modulo (+ k ki) w)])
           (walk d h w r k ri ki f state))
      state))

(define (read filename)
  (list->vector (map (compose list->vector string->list)
                     (file->lines filename))))

(define (main)
  (define data (read "input.txt"))
  (define height (vector-length data))
  (define width (vector-length (vector-ref data 0)))
  (eprintf "height:~a width:~a\n" height width)
  (define (tree-count right down)
    (walk data
          height width
          0 0
          down right
          (λ (c i) (if (eqv? c #\#) (add1 i) i))
          0))
  (define slopes
    '((1 1)
      (3 1)
      (5 1)
      (7 1)
      (1 2)))
  (define tree-counts (map (λ (slope) (apply tree-count slope)) slopes))
  (displayln (apply * tree-counts)))

(main)

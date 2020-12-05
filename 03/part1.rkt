#!/usr/bin/env racket
#lang racket

(define (ref data r k)
  (vector-ref (vector-ref data r) k))

(define (walk d h w r k f state)
  (if (< r h)
      (let ([state (f (ref d r k) state)]
            [r     (+ 1 r)]
            [k     (modulo (+ k 3) w)])
           (walk d h w r k f state))
      state))

(define (read filename)
  (list->vector (map (compose list->vector string->list)
                     (file->lines filename))))

(define (main)
  (define data (read "input.txt"))
  (define height (vector-length data))
  (define width (vector-length (vector-ref data 0)))
  (eprintf "height:~a width:~a\n" height width)
  (define tree-count
    (walk data
          height width
          0 0
          (λ (c i) (if (eqv? c #\#) (add1 i) i))
          0))
  (displayln tree-count))

(main)

#!/usr/bin/env racket
#lang typed/racket

(define-type (Grid α)
  (Vectorof (Vectorof α)))

(define-type Data
  (Grid Char))

(: ref (∀ (α) (-> (Grid α) Integer Integer α)))
(define (ref data r k)
  (vector-ref (vector-ref data r) k))

(: walk (∀ (α) (-> Data Integer Integer Integer Integer (-> Char α α) α α)))
(define (walk d h w r k f state)
  (if (< r h)
      (let ([state (f (ref d r k) state)]
            [r     (+ 1 r)]
            [k     (modulo (+ k 3) w)])
           (walk d h w r k f state))
      state))

(: read (-> Path-String Data))
(define (read filename)
  (list->vector (map (λ ([s : String]) (list->vector (string->list s)))
                     (file->lines filename))))

(: main (-> Void))
(define (main)
  (define data : Data (read "input.txt"))
  (define height : Integer (vector-length data))
  (define width : Integer (vector-length (vector-ref data 0)))
  (eprintf "height:~a width:~a\n" height width)
  (define tree-count : Integer
    (walk data
          height width
          0 0
          (λ ([c : Char] [i : Integer]) : Integer
             (if (eqv? c #\#) (add1 i) i))
          0))
  (displayln tree-count))

(main)

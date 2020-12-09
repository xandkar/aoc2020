#! /usr/bin/env racket
#lang racket

(define instruction/c
  (or/c 'acc 'jmp 'nop))

(define tape/c
  (vectorof (cons/c instruction/c integer?)))

(define/contract (read filepath)
  (-> path-string? tape/c)
  (list->vector
    (map (λ (line)
            (define parts (string-split line))
            (cons
              (string->symbol (first parts))
              (string->number (second parts))))
         (file->lines filepath))))

(define/contract (interpret-1 tape)
  (-> tape/c integer?)
  (define n (vector-length tape))
  (define c (build-vector n (λ (_) 0)))
  (define (++ i)
    (vector-set! c i (add1 (vector-ref c i)))
    (vector-ref c i))
  (define (instruction i) (vector-ref tape i))
  (define (interpret i acc)
    (if (and (< i n) (< (++ i) 2))
        (match (instruction i)
               [(cons 'acc n) (interpret (add1 i) (+ n acc))]
               [(cons 'jmp n) (interpret (+  n i)      acc)]
               [(cons 'nop _) (interpret (add1 i)      acc)])
        acc))
  (interpret 0 0))

(define (main)
  (define tape (read (vector-ref (current-command-line-arguments) 0)))
  (printf "part-1 ~a\n" (interpret-1 tape)))

(main)

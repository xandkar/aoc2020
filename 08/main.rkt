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

(define/contract (interpret tape)
  (-> tape/c (cons/c (or/c 'terminated 'looped) integer?))
  (define len (vector-length tape))
  (define cnt (make-vector len))
  (define (visit i)
    (vector-set! cnt i (add1 (vector-ref cnt i)))
    (vector-ref tape i))
  (define (visited? i) (> (vector-ref cnt i) 0))
  (define (interpret i acc)
    (cond [(>= i len)
           (cons 'terminated acc)]
          [(visited? i)
           (cons 'looped acc)]
          [else
            (match (visit i)
                   [(cons 'acc n) (interpret (+ 1 i) (+ n acc))]
                   [(cons 'jmp n) (interpret (+ n i)      acc)]
                   [(cons 'nop _) (interpret (+ 1 i)      acc)])]))
  (interpret 0 0))

(define (fix tape)
  (define (try i inst-a inst-b n)
    (vector-set! tape i (cons inst-b n))
    (match (interpret tape)
           [(cons 'looped _)
            (begin
              (vector-set! tape i (cons inst-a n))
              (iter (add1 i)))]
           [(cons 'terminated acc)
            acc]))
  (define (iter i)
    (match (vector-ref tape i)
           [(cons 'acc _) (iter (add1 i))]
           [(cons 'jmp n) (try i 'jmp 'nop n)]
           [(cons 'nop n) (try i 'nop 'jmp n)]))
  (iter 0))

(define (main)
  (define tape (read (vector-ref (current-command-line-arguments) 0)))
  (printf "part-1 ~a\n" (cdr (interpret tape)))
  (printf "part-2 ~a\n" (fix tape)))

(main)

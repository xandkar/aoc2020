#! /usr/bin/env racket
#lang racket

(define (read filepath)
  (map string->number (file->lines filepath)))

(define (ordered-pairs xs cmp)
  (define (pairs xs)
    (match xs
           ['() '()]
           [(cons _ '()) '()]
           [(cons x xs) (cons (cons x (car xs))
                              (pairs xs))]))
  (pairs (sort xs cmp)))

(define (intervals xs)
  (map (λ (pair) (- (cdr pair) (car pair))) (ordered-pairs xs <)))

(define (hist xs)
  (foldl (λ (x h) (hash-update h x add1 0)) (hash) xs))

(define (iter f xs)
  (unless (null? xs)
          (void (f (car xs)))
          (iter f (cdr xs))))

(define (count-paths dag root)
  (define count-child
    (let ([known (make-hash)])
         (λ (c) (hash-ref! known c (λ () (count-parent c))))))
  (define (count-parent root)
    (define children (dict-ref dag root))
    (if (null? children)
        1
        (foldl (λ (c total) (+ total (count-child c))) 0 children)))
  (count-parent root))

(define (day10 filepath)
  (define given (read filepath))
  (define implied
    (let ([min 0]
          [max (+ 3 (apply max given))])
         (sort (cons min (cons max given)) <)))
  (define diffs (hash-values (hist (intervals implied))))
  (printf "~a part-1 ~a\n" filepath (apply * diffs))
  (define nexts
    (map (λ (x) (filter (λ (y) (and (>= y x) (<= y (+ 3 x)))) implied)) implied))
  (printf "~a part-2 ~a\n" filepath (count-paths nexts (first implied)))
  (newline))

(define (main)
  (for-each day10 (list "example1.txt"
                        "example2.txt"
                        "input.txt")))

(main)

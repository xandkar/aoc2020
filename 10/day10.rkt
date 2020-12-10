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

(define (main)
  (for-each (λ (filepath)
               (define data (read filepath))
               (define implied
                 (let ([min 0]
                       [max (+ 3 (apply max data))])
                      (cons min (cons max data))))
               (define diffs (hash-values (hist (intervals implied))))
               (printf "~a part-1 ~a ~a\n" filepath (apply * diffs) diffs))
            (list "example1.txt"
                  "example2.txt"
                  "input.txt")))

(main)

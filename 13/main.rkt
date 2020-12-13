#! /usr/bin/env racket
#lang racket

(define/contract (read filepath)
  (-> path-string? (cons/c number? (listof number?)))
  (define lines (file->lines filepath))
  (define time (string->number (first lines)))
  (define buses (filter-map
                  (λ (x) (if (eq? x "x") #f (string->number x)))
                  (string-split (second lines) ",")))
  (cons time buses))

(define (main)
  (for-each
    (λ (input-filepath)
       (define data (read input-filepath))
       (define time (car data))
       (define buses (cdr data))
       (define earliest (car (sort
                               (map (λ (b) (cons b (+ b (* (quotient time b) b))))
                                    buses)
                               (λ (a b) (< (cdr a) (cdr b))))))
       (printf "~a part-1 ~a\n"
               input-filepath
               (* (car earliest) (- (cdr earliest) time)))
       )
    (list "example.txt"
          "input.txt")))

(main)

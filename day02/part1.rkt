#! /usr/bin/env racket
#lang racket

(displayln
  (foldl
    (λ (line valid-count)
       (define parts (string-split line (regexp ": +")))
       (define pw (second parts))
       (define spec (string-split (first parts)))
       (define spec-range (string-split (first spec) "-"))
       (define letter (string-ref (second spec) 0))
       (define lo (string->number (first spec-range)))
       (define hi (string->number (second spec-range)))
       (define occurance-count
         (foldl
           (λ (c cnt) (if (eqv? c letter) (add1 cnt) cnt))
           0
           (string->list pw)))
       (if (and (<= lo occurance-count) (>= hi occurance-count))
           (add1 valid-count)
           valid-count))
    0
    (file->lines "input.txt")))

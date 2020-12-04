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
       (define pos-a (sub1 (string->number (first spec-range))))
       (define pos-b (sub1 (string->number (second spec-range))))
       (define len (string-length pw))
       (if (xor
             (eqv? letter (string-ref pw pos-a))
             (eqv? letter (string-ref pw pos-b)))
           (add1 valid-count)
           valid-count))
    0
    (file->lines "input.txt")))

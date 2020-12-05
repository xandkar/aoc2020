#!/usr/bin/env racket
#lang racket

(define expected
  (list->set
    '("byr"
      "iyr"
      "eyr"
      "hgt"
      "hcl"
      "ecl"
      "pid"
      "cid")))

(define optional
  (list->set '("cid")))

(define required
  (set-subtract expected optional))

(define (read filename)
  (map (λ (passport)
          (map (λ (pair)
                  (string-split pair ":"))
               (string-split passport)))
       (string-split (file->string filename) "\n\n")))

(define (main)
  (define data (read "input.txt"))
  (define valid
    (filter
      (λ (passport)
         (define given (list->set (map car passport)))
         (define missing (set-subtract required given))
         (set-empty? missing))
      data))
  (displayln (length valid)))

(main)

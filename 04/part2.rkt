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

(define (valid-byr? s)
  ; four digits
  ; at least 1920 and at most 2002
  (define n (string->number s))
  (and n (>= n 1920) (<= n 2002)))

(define (valid-iyr? s)
  ; four digits
  ; at least 2010 and at most 2020
  (define n (string->number s))
  (and n (>= n 2010) (<= n 2020)))

(define (valid-eyr? s)
  ; four digits
  ; at least 2020 and at most 2030
  (define n (string->number s))
  (and n (>= n 2020) (<= n 2030)))

(define (valid-hgt? s)
  ; a number followed by either cm or in:
  ;   If cm, the number must be at least 150 and at most 193.
  ;   If in, the number must be at least 59 and at most 76.
  (match (regexp-match (pregexp "^([0-9]+)(cm|in)") s)
         [(list _ n units) (let ([n (string->number n)])
                                (match units
                                       ["in" (and n (>= n 59)  (<= n 76))]
                                       ["cm" (and n (>= n 150) (<= n 193))]))]
         [_ #f]))

(define (valid-hcl? s)
  ; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  (regexp-match? (pregexp "^#[a-f0-9]{6}$") s))

(define (valid-ecl? s)
  ; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  (member s '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")))

(define (valid-pid? s)
  ; pid (Passport ID) - a nine-digit number, including leading zeroes.
  (regexp-match? (pregexp "^[0-9]{9}$") s))

(define (valid-cid? s)
  ; cid (Country ID) - ignored, missing or not.
  #t)

(define (valid? pair)
  (match pair
         [(list "byr" v) (valid-byr? v)]
         [(list "iyr" v) (valid-iyr? v)]
         [(list "eyr" v) (valid-eyr? v)]
         [(list "hgt" v) (valid-hgt? v)]
         [(list "hcl" v) (valid-hcl? v)]
         [(list "ecl" v) (valid-ecl? v)]
         [(list "pid" v) (valid-pid? v)]
         [(list "cid" v) (valid-cid? v)]
         [_              #f]))

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
         (define invalid (filter (λ (pair) (not (valid? pair))) passport))
         (and (set-empty? missing) (empty? invalid)))
      data))
  (displayln (length valid)))

(main)

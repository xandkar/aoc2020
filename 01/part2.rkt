#! /usr/bin/env racket
#lang racket

(define (dedup xs)
  (set->list (list->set xs)))

(define data
  (map string->number (file->lines "input.txt")))

(define products
  (dedup
    (for*/list
      ([x data]
       [y data]
       [z data]
       #:when (= (+ x y z) 2020))
      (* x y z))))

(displayln products)

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
       #:when (= (+ x y) 2020))
      (* x y))))

(displayln products)

#! /usr/bin/env racket
#lang racket

(require math/base)

(define (uniq xs)
  (set->list (list->set xs)))

(define/contract (read filepath)
  (-> path-string?
      (listof (listof (listof char?))))
  (map (λ (s) (map string->list
                   (string-split s "\n")))
       (string-split (file->string filepath) "\n\n")))

(define (main)
  (define data (read "input.txt"))
  (printf "part 1: ~a\n"
          (sum (map (λ (group)
                       (length (uniq (flatten group))))
                    data)))
  (printf "part 2: ~a\n"
          (sum (map (λ (group)
                       (set-count (apply set-intersect (map list->set group))))
                    data))))

(main)

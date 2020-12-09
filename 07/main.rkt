#! /usr/bin/env racket
#lang racket

(define relations/c
  (listof (cons/c string? (listof (cons/c string? natural-number/c)))))

(define/contract (read filepath)
  (-> path-string? relations/c)
  (map (λ (l)
          (match
            (map string-trim (string-split l "bags contain"))
            [(list parent children) #:when (string=? children "no other bags.")
             (cons parent '())]
            [(list parent children)
             (let* ([children
                      (string-split children ", ")]
                    [children
                      (map (λ (str)
                              (let* ([parts
                                       (string-split str)]
                                     [n
                                       (string->number (first parts))]
                                     [child
                                       (string-join (list (second parts)
                                                          (third parts)))])
                                    (cons child n)))
                           children)])
                   (cons parent children))
             ]))
       (file->lines filepath)))

(define/contract (parents relations child)
  (-> relations/c string? (listof string?))
  (filter-map (λ (r) (let ([parent (car r)]
                           [children (cdr r)])
                          (if (member child (map car children))
                              parent
                              #f)))
              relations))

(define/contract (ancestors relations child)
  (-> relations/c string? (listof string?))
  (define (ancestors child)
    (match (parents relations child)
           ['() '()]
           [parents
             (append parents (flatten (map (λ (p) (ancestors p)) parents)))]))
  (set->list (list->set (ancestors child))))

(define (count-decendants relations parent)
  (apply + (map (λ (child-n)
                   (define child (car child-n))
                   (define n-instances (cdr child-n))
                   (define n-decendants
                     (match (count-decendants relations child)
                            [0 0]
                            [n-decendants (* n-instances n-decendants)]))
                   (+ n-instances n-decendants))
                (dict-ref relations parent))))

(define/contract (main)
  (-> void?)
  (define relations (read (vector-ref (current-command-line-arguments) 0)))
  (define target "shiny gold")
  (printf "part-1 ~a\n" (length (ancestors relations target)))
  (printf "part-2 ~a\n" (count-decendants relations target)))

(main)

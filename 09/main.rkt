#! /usr/bin/env racket
#lang racket

(define/contract (read filepath)
  (-> path-string? (vectorof natural-number/c))
  (list->vector (map string->number (file->lines filepath))))

(define (sum-of-previous-two? data x)
  (define found? #f)
  (for* ([i (in-range (vector-length data))]
         [j (in-range (vector-length data))])
        #:break found?
        (when (and (not (= i j))
                   (= x (+ (vector-ref data i) (vector-ref data j))))
              (set! found? #t)))
  found?)

(define (find-invalid data preamble)
  (define result 'none)
  (for ([i (in-range preamble (vector-length data))]
        #:break (pair? result))
       (let ([x (vector-ref data i)]
             [previous (vector-take-right (vector-take data i) preamble)])
            (when (not (sum-of-previous-two? previous x))
                  (set! result (cons 'some x)))))
  result)

(define (find-seq-addends data target-sum)
  (define (try data i sum xs)
    (if (>= i (vector-length data))
        '()
        (let* ([x  (vector-ref data i)]
               [sum (+ sum x)])
              (cond [(= sum target-sum) (cons x xs)]
                    [(> sum target-sum) '()]
                    [(< sum target-sum) (try data (add1 i) sum (cons x xs))]))))
  (define result '())
  (for ([start (in-range 0 (vector-length data))]
        #:break (not (empty? result)))
       (set! result (reverse (try (vector-drop data start) 0 0 '()))))
  result)

(define (sum-of-largest-and-smallest-two xs)
  (+ (car (sort xs >))
     (car (sort xs <))))

(define (main)
  (for-each (λ (spec)
               (define preamble (car spec))
               (define filepath (cdr spec))
               (define data (read filepath))
               (match (find-invalid data preamble)
                      [(cons 'some n)
                       (printf "~a part-1 ~a\n"
                               filepath n)
                       (printf "~a part-2 ~v\n"
                               filepath (sum-of-largest-and-smallest-two
                                          (find-seq-addends data n)))
                       ]))
            (list (cons 5 "example.txt")
                  (cons 25 "input.txt"))))

(main)

#! /usr/bin/env racket
#lang racket

(struct pt (r k))
(struct dim (nr nk))

(define state/c (or/c 'floor 'empty 'occupied))
(define board/c (vectorof (vectorof state/c)))

(define/contract (read filepath)
  (-> path-string? board/c)
  (define (char->state c)
    (match c
           [#\# 'occupied]
           [#\L 'empty]
           [#\. 'floor]))
  (list->vector (map (λ (line)
                        (list->vector (map char->state (string->list line))))
                     (file->lines filepath))))

(define (dims board)
  (dim (vector-length board)
       (vector-length (vector-ref board 0))))

(define (board-ref b p)
  (vector-ref (vector-ref b (pt-r p)) (pt-k p)))

(define (board-set! b p value)
  (vector-set! (vector-ref b (pt-r p)) (pt-k p) value))

(define (inbounds? p nr nk)
  (and (and (>= (pt-r p) 0) (< (pt-r p) nr))
       (and (>= (pt-k p) 0) (< (pt-k p) nk))))

(define moore-offsets
  (list (pt -1 -1) (pt -1 0) (pt -1 1)
        (pt  0 -1)           (pt  0 1)
        (pt  1 -1) (pt  1 0) (pt  1 1)))

(define (pt+ p1 p2)
  (pt (+ (pt-r p1)
         (pt-r p2))
      (+ (pt-k p1)
         (pt-k p2))))

(define (hood p)
  (map (λ (o) (pt+ p o)) moore-offsets))

(define (neighbors board point)
  (define d (dims board))
  (define points (filter (λ (p) (inbounds? p (dim-nr d) (dim-nk d))) (hood point)))
  (map (λ (p) (board-ref board p)) points))

(define (count-neighbors-occupied b p)
  (apply + (map (λ (s) (match s ['occupied 1] [_ 0])) (neighbors b p))))

(define/contract (count-board-occupied b)
  (-> board/c number?)
  (define n-occupied 0)
  (define d (dims b))
  (for* ([r (dim-nr d)]
         [k (dim-nk d)]
         #:when (eq? 'occupied (board-ref b (pt r k))))
        (set! n-occupied (add1 n-occupied)))
  n-occupied)

(define (board d)
  (build-vector (dim-nr d) (λ (_) (make-vector (dim-nk d) 'floor))))

(define (next b0)
  (define d (dims b0))
  (define b1 (board d))
  (define changes 0)
  (for* ([r (dim-nr d)]
         [k (dim-nk d)])
        (let* ([p  (pt r k)]
               [s0 (board-ref b0 p)]
               [s1 (match (cons s0 (count-neighbors-occupied b0 p))
                          [(cons 'empty 0) 'occupied]
                          [(cons 'occupied n) #:when (>= n 4) 'empty]
                          [_ s0])])
              (when (not (eq? s0 s1)) (set! changes (add1 changes)))
              (board-set! b1 p s1)))
  (cons b1 changes))

(define (find-stabilized board)
  (match (next board)
         [(cons board changes) #:when (> changes 0) (find-stabilized board)]
         [(cons board _) board]))

(define (main)
  (for-each
    (λ (input-filepath)
       (define data (read input-filepath))
       (define stabilized (find-stabilized data))
       (define n-occupied (count-board-occupied stabilized))
       (printf "~a part-1 ~a\n" input-filepath n-occupied))
    (list "example.txt"
          "input.txt")))

(main)

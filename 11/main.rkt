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

(define (offset dir depth)
  (define d depth)
  (match dir
         ['nw (pt (- d) (- d))] ['n (pt (- d) 0)] ['ne (pt (- d) d)]
         ['w  (pt    0  (- d))]                   ['e  (pt    0  d)]
         ['sw (pt    d  (- d))] ['s (pt     d 0)] ['se (pt    d  d)]))

(define moore-dirs
  '(nw n ne
    w    e
    sw s se))

(define moore-offsets
  (map (λ (d) (offset d 1)) moore-dirs))

(define (pt+ p1 p2)
  (pt (+ (pt-r p1)
         (pt-r p2))
      (+ (pt-k p1)
         (pt-k p2))))

(define (look-for-occupied b p1 [max-depth #f])
  (define d (dims b))
  (define nr (dim-nr d))
  (define nk (dim-nk d))
  (define (look depth dirs count)
    (let* ([dirs-count
             (foldl (λ (dir dirs-count)
                       (define dirs (car dirs-count))
                       (define count (cdr dirs-count))
                       (define p2 (pt+ p1 (offset dir depth)))
                       (if (inbounds? p2 nr nk)
                           (match (board-ref b p2)
                                  ['floor    (cons (cons dir dirs)       count)]
                                  ['empty    (cons           dirs        count)]
                                  ['occupied (cons           dirs  (add1 count))])
                           dirs-count)
                       )
                    (cons '() count)
                    dirs)]
           [dirs (car dirs-count)]
           [count (cdr dirs-count)])
          (cond
            [(null? dirs)
             count]
            [(and max-depth (>= depth max-depth))
             count]
            [else
              (look (add1 depth) dirs count)])))
  (look 1 moore-dirs 0))

(define (board-occupied b)
  (apply + (map (λ (s) (match s ['occupied 1] [_ 0]))
                (flatten (vector->list (vector-map vector->list b))))))

(define (board d)
  (build-vector (dim-nr d) (λ (_) (make-vector (dim-nk d) 'floor))))

(define (next b0 max-occupied [max-depth #f])
  (define d (dims b0))
  (define b1 (board d))
  (define changes 0)
  (for* ([r (dim-nr d)]
         [k (dim-nk d)])
        (let* ([p  (pt r k)]
               [s0 (board-ref b0 p)]
               [s1 (match (cons s0 (look-for-occupied b0 p max-depth))
                          [(cons 'empty 0) 'occupied]
                          [(cons 'occupied n) #:when (>= n max-occupied) 'empty]
                          [_ s0])])
              (when (not (eq? s0 s1)) (set! changes (add1 changes)))
              (board-set! b1 p s1)))
  (cons b1 changes))

(define (find-stabilized board mo md)
  (match (next board mo md)
         [(cons board changes) #:when (> changes 0) (find-stabilized board mo md)]
         [(cons board _) board]))

(define (main)
  (for-each
    (λ (input-filepath)
       (define data (read input-filepath))
       (printf "~a part-1 ~a\n"
               input-filepath
               (board-occupied (find-stabilized data 4 1)))
       (printf "~a part-2 ~a\n"
               input-filepath
               (board-occupied (find-stabilized data 5 #f))))
    (list "example.txt"
          "input.txt")))

(main)

#lang racket

(require "../base.rkt")

(define *n* 50)
(define *size* (exact-round (/ 500 *n*)))

; Data generation

(define (random-square)
  (match (random 2)
    [0 '(sq white)]
    [1 '(sq black)]))

(define (random-squares n)
  (for/list ([i n])
    (for/list ([i n])
      (random-square))))

(define cache (make-hash))
(define (bitmap:draw-square c)
  (hash-ref! cache (cons *size* c)
             (thunk (bitmap-rectangle *size* *size* #:color (cons c 'solid)))))

(define (bitmap:render block)
  (bitmap-vl-append*
   (for/list ([row block])
     (bitmap-ht-append*
      (for/list ([piece row])
        (match piece [`(sq ,c) (bitmap:draw-square c)]))))))

(define (bitmap:render:table block)
  (apply bitmap-table *n* '(ct) '(lc) '(0) '(0)
         (for*/list ([row block] [piece row])
           (match piece [`(sq ,c) (bitmap:draw-square c)]))))

; Timings
(define b0 (random-squares *n*))

(displayln "bitmap-*-append...")
(void (time (bitmap:render b0)))
(displayln "bitmap-table...")
(bitmap-scale (time (bitmap:render:table b0)) 1/2)

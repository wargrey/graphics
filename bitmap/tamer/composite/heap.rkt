#lang racket

(require "../../constructor.rkt")
(require "../../composite.rkt")
(require "../../font.rkt")
(require "../../color.rkt")

(require "../../digitama/unsafe/convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define index->node
  (λ [i]
    (define datum (bitmap-text (number->string (add1 i))))
    (define r (quotient (bitmap-height datum) 2))
    (define frame (bitmap-circle r))
    
    (bitmap-cc-superimpose frame datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define nodes (build-list 15 index->node))
  
  (for/list ([ary (in-range 1 4)])
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (time (bitmap-heap nodes #:ary ary))))

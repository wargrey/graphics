#lang typed/racket/base

(provide (all-defined-out))

(require "block.rkt")
(require "style.rkt")
(require "types.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (M) default-mtx-header-fallback-construct : Mtx-Header->Block
  (lambda [id brief style width height direction indices]
    (cond [(mtx-row-header-style? (car style)) (mtx-block-row-header id brief style width height direction indices)]
          [(mtx-col-header-style? (car style)) (mtx-block-col-header id brief style width height direction indices)]
          [(mtx-corner-style? (car style)) (mtx-block-corner id brief style width height direction indices)])))

(define #:forall (M) default-mtx-entry-fallback-construct : (Mtx-Entry->Block M)
  (lambda [self brief style width height direction indices]
    (define id : Symbol (car self))
    (cond [(mtx-entry-style? (car style)) (mtx-block-entry id brief style width height direction indices)]
          [(mtx-hole-style? (car style)) (mtx-block-hole id brief style width height direction indices)]
          [(mtx-mask-style? (car style)) (mtx-block-mask id brief style width height direction indices)])))

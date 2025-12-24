#lang typed/racket/base

(provide (all-defined-out))

(require "self.rkt")
(require "block.rkt")
(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (M) default-mtx-header-fallback-construct : Mtx-Header->Block
  (lambda [id brief style width height direction indices]
    (cond [(mtx-row-header-style? style) (mtx-block-row-header id brief style width height direction indices)]
          [(mtx-col-header-style? style) (mtx-block-col-header id brief style width height direction indices)]
          [(mtx-corner-style? style) (mtx-block-corner id brief style width height direction indices)])))

(define #:forall (M) default-mtx-entry-fallback-construct : (Mtx-Entry->Block M)
  (lambda [self brief style width height direction indices]
    (define id : Symbol (car self))
    (cond [(mtx-entry-style? style) (mtx-block-entry id brief style width height direction indices)]
          [(mtx-hole-style? style) (mtx-block-hole id brief style width height direction indices)]
          [(mtx-mask-style? style) (mtx-block-mask id brief style width height direction indices)])))

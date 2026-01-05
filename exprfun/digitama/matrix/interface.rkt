#lang typed/racket/base

(provide (all-defined-out))

(require "slot.rkt")
(require "style.rkt")
(require "types.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (M) default-mtx-header-fallback-construct : Mtx-Header->Slot
  (lambda [id anchor term style width height direction indices]
    (cond [(mtx-row-header-style? (car style)) (mtx-slot-row-header id term style width height direction indices)]
          [(mtx-col-header-style? (car style)) (mtx-slot-col-header id term style width height direction indices)]
          [(mtx-corner-style? (car style)) (mtx-slot-corner id term style width height direction indices)])))

(define #:forall (M) default-mtx-entry-fallback-construct : (Mtx-Entry->Slot M)
  (lambda [id self term style width height direction indices]
    (cond [(mtx-entry-style? (car style)) (mtx-slot-entry id term style width height direction indices)]
          [(mtx-hole-style? (car style)) (mtx-slot-hole id term style width height direction indices)]
          [(mtx-mask-style? (car style)) (mtx-slot-mask id term style width height direction indices)])))

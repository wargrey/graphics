#lang typed/racket/base

(provide (all-defined-out))

(require "slot.rkt")
(require "style.rkt")
(require "types.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (M) default-mtx-header-fallback-construct : Mtx-Header->Slot
  (lambda [id anchor term style width height direction indices]
    (expr-slot-case style
      [(mtx-row-header-style?) (mtx-slot-row-header id term style width height direction indices)]
      [(mtx-col-header-style?) (mtx-slot-col-header id term style width height direction indices)]
      [(mtx-corner-style?) (mtx-slot-corner id term style width height direction indices)])))

(define #:forall (M) default-mtx-entry-fallback-construct : (Mtx-Entry->Slot M)
  (lambda [id self term style width height direction indices]
    (expr-slot-case style
      [(mtx-entry-style?) (mtx-slot-entry id term style width height direction indices)]
      [(mtx-hole-style?) (mtx-slot-hole id term style width height direction indices)]
      [(mtx-mask-style?) (mtx-slot-mask id term style width height direction indices)])))

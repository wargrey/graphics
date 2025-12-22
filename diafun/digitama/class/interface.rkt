#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require "block.rkt")
(require "../track/interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diacls-block-fallback-construct : Dia-Anchor->Block
  (lambda [id label style width height direction subtype]
    (case/eq (object-name style)
             [(diacls-interface-style) (diacls-block-interface id label style width height direction subtype)]
             [(diacls-class-style)     (diacls-block-class id label style width height direction subtype)])))

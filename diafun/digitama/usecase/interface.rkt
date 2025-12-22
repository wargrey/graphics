#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require "block.rkt")
(require "../track/interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diauc-block-fallback-construct : Dia-Anchor->Block
  (lambda [id label style width height direction subtype]
    (case/eq (object-name style)
             [(diamtx-body-style) (diauc-block-ucase id label style width height direction subtype)]
             [(diamtx-row-head-style) (diauc-block-actor id label style width height direction subtype)]
             [(diamtx-column-head-style) (diauc-block-ucase id label style width height direction subtype)])))

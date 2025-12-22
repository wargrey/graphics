#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require "self.rkt")
(require "block.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diamtx-block-fallback-construct : Dia-Matrix-Id->Block
  (lambda [id label style width height direction indices]
    (case/eq (object-name style)
             [(diamtx-entry-style) (diamtx-block-entry id label style width height direction indices)]
             [(diamtx-hole-style) (diamtx-block-hole id label style width height direction indices)]
             [(diamtx-mask-style) (diamtx-block-mask id label style width height direction indices)]
             [(diamtx-row-header-style) (diamtx-block-row-header id label style width height direction indices)]
             [(diamtx-column-header-style) (diamtx-block-col-header id label style width height direction indices)]
             [(diamtx-corner-style) (diamtx-block-corner id label style width height direction indices)])))

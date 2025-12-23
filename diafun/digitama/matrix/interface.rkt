#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require "self.rkt")
(require "block.rkt")
(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Matrix-Block-Type-Abbr (U 'entry 'hole 'mask 'rhdr 'chdr 'cnr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diamtx-block-identify : (-> Symbol Dia-Matrix-Block-Type-Abbr Index Index Dia-Block-Style)
  (lambda [self type r c]
    (case/eq type
      [(entry) (dia-block-style-construct self (default-diamtx-entry-style-make) make-diamtx-entry-style (cons r c))]
      [(hole) (dia-block-style-construct self (default-diamtx-hole-style-make) make-diamtx-hole-style (cons r c))]
      [(mask) (dia-block-style-construct self (default-diamtx-mask-style-make) make-diamtx-mask-style (cons r c))]
      [(rhdr) (dia-block-style-construct self (default-diamtx-row-header-style-make) make-diamtx-row-header-style (cons r c))]
      [(chdr) (dia-block-style-construct self (default-diamtx-col-header-style-make) make-diamtx-col-header-style (cons r c))]
      [else (dia-block-style-construct self (default-diamtx-corner-style-make) make-diamtx-corner-style (cons r c))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diamtx-block-fallback-construct : Dia-Matrix-Id->Block
  (lambda [id label style width height direction indices]
    (cond [(diamtx-entry-style? style) (diamtx-block-entry id label style width height direction indices)]
          [(diamtx-hole-style? style) (diamtx-block-hole id label style width height direction indices)]
          [(diamtx-mask-style? style) (diamtx-block-mask id label style width height direction indices)]
          [(diamtx-row-header-style? style) (diamtx-block-row-header id label style width height direction indices)]
          [(diamtx-col-header-style? style) (diamtx-block-col-header id label style width height direction indices)]
          [(diamtx-corner-style? style) (diamtx-block-corner id label style width height direction indices)])))

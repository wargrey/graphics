#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Matrix-Block-Type-Abbr (U 'entry 'hole 'mask 'rh 'ch 'cnr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diamtx-block-identify : (-> Symbol Dia-Matrix-Block-Type-Abbr Index Index Dia-Block-Style)
  (lambda [self type r c]
    (case/eq type
             [(entry) (dia-block-style-construct self (default-diamtx-entry-style-make) make-diamtx-entry-style (cons r c))]
             [(hole) (dia-block-style-construct self (default-diamtx-hole-style-make) make-diamtx-hole-style (cons r c))]
             [(mask) (dia-block-style-construct self (default-diamtx-mask-style-make) make-diamtx-mask-style (cons r c))]
             [(rh) (dia-block-style-construct self (default-diamtx-row-header-style-make) make-diamtx-row-header-style (cons r c))]
             [(ch) (dia-block-style-construct self (default-diamtx-column-header-style-make) make-diamtx-column-header-style (cons r c))]
             [else (dia-block-style-construct self (default-diamtx-corner-style-make) make-diamtx-corner-style (cons r c))])))

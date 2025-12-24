#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require "self.rkt")
(require "block.rkt")
(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Mtx-Block-Type (U 'entry 'hole 'mask 'rhdr 'chdr 'cnr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-mtx-block-identify : (-> Any Mtx-Block-Type Mtx-Indices Dia-Block-Style)
  (lambda [self type indices]
    (case/eq type
      [(entry) (dia-block-style-construct self (default-mtx-entry-style-make) make-mtx-entry-style indices)]
      [(hole) (dia-block-style-construct self (default-mtx-hole-style-make) make-mtx-hole-style indices)]
      [(mask) (dia-block-style-construct self (default-mtx-mask-style-make) make-mtx-mask-style indices)]
      [(rhdr) (dia-block-style-construct self (default-mtx-row-header-style-make) make-mtx-row-header-style indices)]
      [(chdr) (dia-block-style-construct self (default-mtx-col-header-style-make) make-mtx-col-header-style indices)]
      [else (dia-block-style-construct self (default-mtx-corner-style-make) make-mtx-corner-style indices)])))

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

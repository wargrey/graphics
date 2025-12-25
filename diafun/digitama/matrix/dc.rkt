#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)
(require geofun/digitama/self)

(require "self.rkt")
(require "style.rkt")

(require "../block/dc.rkt")
(require "../block/style.rkt")
(require "../interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Mtx-Block-Type (U 'entry 'hole 'mask 'rhdr 'chdr 'cnr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-mtx-style-make : (-> Any Mtx-Block-Type Mtx-Indices Dia-Block-Style)
  (lambda [self type indices]
    (case/eq type
      [(entry) (dia-block-style-construct self (default-mtx-entry-style-make) make-mtx-entry-style indices)]
      [(hole) (dia-block-style-construct self (default-mtx-hole-style-make) make-mtx-hole-style indices)]
      [(mask) (dia-block-style-construct self (default-mtx-mask-style-make) make-mtx-mask-style indices)]
      [(rhdr) (dia-block-style-construct self (default-mtx-row-header-style-make) make-mtx-row-header-style indices)]
      [(chdr) (dia-block-style-construct self (default-mtx-col-header-style-make) make-mtx-col-header-style indices)]
      [else (dia-block-style-construct self (default-mtx-corner-style-make) make-mtx-corner-style indices)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) dia-mtx-block-make : (-> T Dia-Block-Style-Layers Mtx-Indices (Option Geo) (Option Flonum)
                                              (Option (Dia-Anchor->Block* T Mtx-Indices)) (Dia-Anchor->Block* T Mtx-Indices)
                                              (Option Dia:Block))
  (lambda [self style indices brief direction make-block fallback-block ]
    (define-values (width height) (dia-block-smart-size brief style))
    (define block : (U Dia:Block Void False)
      (cond [(not make-block) (void)]
            [else (make-block self brief style width height direction indices)]))
    
    (if (void? block)
        (let ([fallback-block (fallback-block self brief style width height direction indices)])
          (and (dia:block? fallback-block)
               fallback-block))
        block)))

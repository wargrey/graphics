#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/convert)

(require "self.rkt")

(require "../block/dc.rkt")
(require "../block/style.rkt")
(require "../interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) dia-mtx-block-make : (-> T Dia-Block-Style Mtx-Indices (Option Geo) (Option Flonum)
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

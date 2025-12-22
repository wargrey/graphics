#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/convert)

(require "self.rkt")

(require "../block/dc.rkt")
(require "../block/style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-matrix-block-make : (-> Symbol Dia-Block-Style Dia-Matrix-Urgent-Datum (Option Real) (Option Real)
                                    (Option Dia-Matrix-Id->Block) (Option Geo) Dia-Matrix-Id->Block (Option Flonum)
                                    (Option Dia:Block))
  (lambda [id style indices alt-width alt-height make-block brief fallback-block direction]
    (define-values (width height) (dia-block-smart-size brief style #:width alt-width #:height alt-height))
    (define block : (U Dia:Block Void False)
      (cond [(not make-block) (void)]
            [else (make-block id brief style width height direction indices)]))
    
    (if (void? block)
        (let ([fallback-block (fallback-block id brief style width height direction indices)])
          (and (dia:block? fallback-block)
               fallback-block))
        block)))

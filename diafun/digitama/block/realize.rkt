#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)
(require geofun/font)

(require geofun/digitama/self)
(require geofun/digitama/track/anchor)
(require geofun/digitama/richtext/self)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/position)

(require "dc.rkt")
(require "style.rkt")
(require "interface.rkt")
(require "backstop.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S M) dia-block-realize* : (-> (Dia-Block-Identifier S M) (Dia-Block-Builder S M)
                                                (Dia-Block-Typesetter S) (Option (Dia-Block-Describer S M)) Dia-Block-Backstop-Style
                                                Geo-Anchor-Name (Option Flonum) Nonnegative-Flonum (Option Nonnegative-Flonum) 
                                                (Option Dia:Block))
  (lambda [block-identifier make-block make-caption block-desc backstop-style anchor direction scale opacity]
    (define blk-info (block-identifier anchor))
    
    (and blk-info
         (let-values ([(text metadata style) (values (car blk-info) (cadr blk-info) (caddr blk-info))])
           (and style
                (let ([style-spec ((inst make-dia-block-style-spec S) #:custom style #:backstop backstop-style #:scale scale #:opacity opacity)])
                  (parameterize ([default-font-metrics (λ [[unit : Font-Unit]] (font-metrics-ref (dia-block-resolve-font style-spec) unit))])
                    (define-values (width height) (dia-block-resolve-size style-spec))

                    (define maybe-caption : Geo-Maybe-Rich-Text
                      (cond [(not block-desc) (void)]
                            [(hash? block-desc) (hash-ref block-desc anchor void)]
                            [else (block-desc anchor text style-spec metadata)]))

                    (define block : Dia-Maybe-Block
                      (let ([caption-text (if (void? maybe-caption) text maybe-caption)]
                            [id (geo-anchor->symbol anchor)])
                        (make-block id (and maybe-caption (make-caption id caption-text style-spec))
                                    style-spec width height direction metadata)))
                           
                    (and (dia:block? block) block))))))))

(define #:forall (S M) dia-block-layer-realize : (-> (Dia-Block-Identifier S M) (Dia-Block-Builder S M)
                                                     (Dia-Block-Typesetter S) (Option (Dia-Block-Describer S M)) Dia-Block-Backstop-Style
                                                     Geo-Anchor-Name Float-Complex (Option Flonum)
                                                     Nonnegative-Flonum (Option Nonnegative-Flonum)
                                                     (Option (GLayerof Dia:Block)))
  (lambda [block-identifier make-block make-caption block-desc backstop-style anchor position direction scale opacity]
    (define maybe-block
      (dia-block-realize* block-identifier make-block make-caption block-desc backstop-style
                          anchor direction scale opacity))

    (and maybe-block
         (geo-own-pin-layer 'cc position maybe-block 0.0+0.0i))))

(define #:forall (S M) dia-block-realize : (-> (Dia-Block-Factory S M) (Option (Dia-Block-Describer S M)) Geo-Anchor-Name (Option Flonum)
                                               Nonnegative-Flonum (Option Nonnegative-Flonum)
                                               (Option Dia:Block))
  (lambda [self block-desc anchor direction scale opacity]
    (dia-block-realize* (dia-block-factory-identifier self)
                        (dia-block-builder-compose (dia-block-factory-builder self)
                                                   (dia-block-factory-fallback-builder self))
                        (dia-block-typesetter-compose (dia-block-factory-typesetter self))
                        block-desc
                        ((dia-block-factory-λbackstop-style self))
                        anchor direction scale opacity)))

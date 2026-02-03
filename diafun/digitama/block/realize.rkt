#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)
(require geofun/font)
(require geofun/track)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/position)
(require geofun/digitama/track/anchor)

(require "dc.rkt")
(require "style.rkt")
(require "interface.rkt")
(require "backstop.rkt")

(require "../decoration/note/self.rkt")
(require "../decoration/note/realize.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S M) dia-block-realize* : (-> (Dia-Block-Identifier S M) (Dia-Block-Builder S M) (Dia-Block-Typesetter S)
                                                (Option (Dia-Block-Link-Root-Style S)) Dia-Block-Backstop-Style
                                                (Option (Dia-Block-Describer S M)) (Option Dia-Note-Factory) (Option Dia-Note-Describer)
                                                Geo-Anchor-Name (Option Flonum) (Option Nonnegative-Flonum) (Option Nonnegative-Flonum) 
                                                (Option Dia:Block))
  (lambda [block-identifier make-block make-caption root-style backstop-style block-desc note-factory note-desc anchor direction scale opacity]
    (define text : String (geo-anchor->string anchor))
    (define size : Index (string-length text))
    (define ch0 : (Option Char) (and (> size 0) (string-ref text 0)))

    (and (> size 0)
         (let ([ch0 (string-ref text 0)])
           (cond [(eq? ch0 #\.) #false]
                 [(not (and (eq? ch0 #\/) (regexp-match? #px"^//" text)))
                  (let ([blk-info (block-identifier anchor text size)])
                    (and blk-info
                         (let-values ([(text metadata style) (values (car blk-info) (cadr blk-info) (caddr blk-info))])
                           (and style
                                (let ([style-spec ((inst make-dia-block-style-spec S) #:custom style #:backstop backstop-style
                                                                                      #:root (and root-style (root-style style))
                                                                                      #:scale scale #:opacity opacity)])
                                  (parameterize ([default-font-metrics (λ [[unit : Font-Unit]] (font-metrics-ref (dia-block-resolve-font style-spec) unit))])
                                    (define-values (width height) (dia-block-resolve-size style-spec))
                                    
                                    (let* ([id (geo-anchor->symbol anchor)]
                                           [maybe-caption (cond [(not text) #false]
                                                                [(not block-desc) (void)]
                                                                [(hash? block-desc) (hash-ref block-desc anchor void)]
                                                                [else (block-desc anchor text style-spec metadata)])]
                                           [caption (and maybe-caption (make-caption id (if (void? maybe-caption) text maybe-caption) style-spec))]
                                           [block (make-block id caption style-spec width height direction metadata)])
                                      (and (dia:block? block) block))))))))]
                 [(or note-factory) (dia-note-block-realize anchor text size note-factory note-desc direction scale opacity)]
                 [else #false])))))

(define #:forall (S M) dia-block-layer-realize : (-> (Dia-Block-Identifier S M) (Dia-Block-Builder S M) (Dia-Block-Typesetter S)
                                                     (Option (Dia-Block-Link-Root-Style S)) Dia-Block-Backstop-Style
                                                     (Option (Dia-Block-Describer S M)) (Option Dia-Note-Factory) (Option Dia-Note-Describer)
                                                     Geo-Anchor-Name Float-Complex (Option Flonum) (Option Nonnegative-Flonum) (Option Nonnegative-Flonum)
                                                     (Option (GLayerof Dia:Block)))
  (lambda [block-identifier make-block make-caption root-style backstop-style block-desc note-factory note-desc anchor position direction scale opacity]
    (define maybe-block
      (dia-block-realize* block-identifier make-block make-caption
                          root-style backstop-style block-desc
                          note-factory note-desc
                          anchor direction scale opacity))

    (and maybe-block
         (geo-own-pin-layer 'cc position maybe-block 0.0+0.0i))))

(define #:forall (S M) dia-block-realize : (-> (Dia-Block-Factory S M) (Option (Dia-Block-Describer S M))
                                               (Option Dia-Note-Factory) (Option Dia-Note-Describer)
                                               Any (Option Flonum) Nonnegative-Real (Option Nonnegative-Real)
                                               (Option Dia:Block))
  (lambda [self block-desc note-factory note-desc anchor direction scale opacity]
    (parameterize ([current-master-track #false])
      (dia-block-realize* (dia-block-factory-identifier self)
                          (dia-block-builder-compose (dia-block-factory-builder self)
                                                     (dia-block-factory-fallback-builder self))
                          (dia-block-typesetter-compose (dia-block-factory-typesetter self))
                          (dia-block-factory-λroot-style self)
                          ((dia-block-factory-λbackstop-style self))
                          block-desc note-factory note-desc
                          (cond [(symbol? anchor) anchor]
                                [(keyword? anchor) anchor]
                                [(string? anchor) (string->symbol anchor)]
                                [else (string->symbol (format "~a" anchor))])
                          direction
                          (and (> scale 0.0) (not (= scale 1.0)) (real->double-flonum scale))
                          (and opacity (< opacity 1.0) (real->double-flonum opacity))))))

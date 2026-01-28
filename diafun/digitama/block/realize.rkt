#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)
(require geofun/font)
(require geofun/track)

(require geofun/digitama/track/anchor)
(require geofun/digitama/layer/type)
(require geofun/digitama/layer/position)

(require "dc.rkt")
(require "style.rkt")
(require "interface.rkt")
(require "backstop.rkt")

(require "../decoration/note.rkt")

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
                                           [maybe-caption (cond [(not block-desc) (void)]
                                                                [(hash? block-desc) (hash-ref block-desc anchor void)]
                                                                [else (block-desc anchor text style-spec metadata)])]
                                           [caption (and maybe-caption (make-caption id (if (void? maybe-caption) text maybe-caption) style-spec))]
                                           [block (make-block id caption style-spec width height direction metadata)])
                                      (and (dia:block? block) block))))))))]
                 [(not note-factory) #false]
                 [else (dia-note-realize* anchor text size note-factory note-desc scale opacity)])))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-note-realize* : (-> Geo-Anchor-Name String Positive-Index Dia-Note-Factory (Option Dia-Note-Describer)
                                (Option Nonnegative-Flonum) (Option Nonnegative-Flonum) (Option Dia:Block:Note))
  (lambda [anchor text size note-factory note-desc scale opacity]
    (define make-note (dia-note-factory-builder note-factory))
    
    (and make-note
         (let* ([text (substring text 2 size)]
                [note-info ((inst dia-block-info Dia-Note-Style Dia-Note-Metadata) anchor text make-dia-note-style default-dia-note-theme-adjuster #false)])
           (let*-values ([(text metadata style) (values (car note-info) (cadr note-info) (caddr note-info))])
             (and style
                  (let* ([backstop-style ((dia-note-factory-λbackstop-style note-factory))]
                         [typeset (or (dia-note-factory-typesetter note-factory) dia-note-typeset)]
                         [style-spec ((inst make-dia-block-style-spec Dia-Note-Style) #:custom style #:backstop backstop-style #:scale scale #:opacity opacity)])
                    (parameterize ([default-font-metrics (λ [[unit : Font-Unit]] (font-metrics-ref (dia-block-resolve-font style-spec) unit))])
                      (define-values (min-width min-height) (dia-block-resolve-size style-spec))
                      
                      (let* ([id (geo-anchor->symbol anchor)]
                             [maybe-desc (cond [(not note-desc) (void)]
                                               [(hash? note-desc) (hash-ref note-desc anchor void)]
                                               [else (note-desc anchor text style-spec metadata)])])
                        (make-note id
                                   (and maybe-desc
                                        (typeset id (if (void? maybe-desc) text maybe-desc)
                                                 style-spec))
                                   style-spec min-width min-height metadata))))))))))

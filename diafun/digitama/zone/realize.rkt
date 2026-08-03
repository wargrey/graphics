#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require digimon/measure)
(require digimon/function)

(require geofun/font)
(require geofun/digitama/self)
(require geofun/digitama/geometry/sides)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/sticker)

(require geofun/digitama/path/dc)
(require geofun/digitama/track/self)
(require geofun/digitama/track/anchor)

(require "self.rkt")
(require "style.rkt")
(require "interface.rkt")

(require "../stereotype.rkt")
(require "../block/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-rubber-zone-realize
  : (-> Geo:Track:Zone:Rubber (HashTable Geo-Anchor-Name Float-Complex) (Immutable-HashTable Geo-Anchor-Name (Option (GLayerof Dia:Block)))
        (Listof (GLayerof Geo-Path)) (Option Nonnegative-Flonum)
        (Dia-Zone-Identifier Dia-Zone-Style) (Dia-Zone-Typesetter Dia-Zone-Style) (Dia-Zone-Builder Dia-Zone-Style)
        Dia-Zone-Backstop-Style (Option Dia-Zone-Describer)
        (Option (GLayerof Geo)))
  (lambda [self positions blockdb tracks opacity identify typeset build backstop-style zone-desc]
    (let resolve-zone-boundary ([lx : Flonum +inf.0] [lfixed? : Boolean #false]
                                [ty : Flonum +inf.0] [tfixed? : Boolean #false]
                                [rx : Flonum -inf.0] [rfixed? : Boolean #false]
                                [by : Flonum -inf.0] [bfixed? : Boolean #false]
                                [anchors : (Listof Geo-Anchor-Name) (geo:track:zone:rubber-anchors self)])
      (if (pair? anchors)
          (let*-values ([(anchor rest) (values (car anchors) (cdr anchors))]
                        [(rect) (dia-anchor->boundary anchor blockdb positions)])
            (if (and rect)
                (let*-values ([(pin?) (dia-anchor-pin? anchor)]
                              [(x y) (values (vector-ref rect 0) (vector-ref rect 1))]
                              [(x+w y+h) (values (+ x (vector-ref rect 2)) (+ y (vector-ref rect 3)))]
                              [(lx++ lfixed?) (if (<= x lx) (values x (or pin? lfixed?)) (values lx lfixed?))]
                              [(rx++ rfixed?) (if (>= x+w rx) (values x+w (or pin? rfixed?)) (values rx rfixed?))]
                              [(ty++ tfixed?) (if (<= y ty) (values y (or pin? tfixed?)) (values ty tfixed?))]
                              [(by++ bfixed?) (if (>= y+h by) (values y+h (or pin? bfixed?)) (values by bfixed?))])
                  (resolve-zone-boundary lx++ lfixed? ty++ tfixed? rx++ rfixed? by++ bfixed? rest))
                (resolve-zone-boundary lx lfixed? ty tfixed? rx rfixed? by bfixed? rest)))
          (let-values ([(width height) (values (- rx lx) (- by ty))])
            (and (>= width 0.0) (>= height 0.0)
                 (dia-zone-realize self width height (make-rectangular lx ty) opacity
                                   identify typeset build backstop-style zone-desc
                                   (vector-immutable (not tfixed?) (not rfixed?) (not bfixed?) (not lfixed?))
                                   make-dia-flex-zone-style default-dia-flex-zone-theme-adjuster)))))))

(define dia-fixed-zone-realize : (-> Geo:Track:Zone:Fixed (HashTable Geo-Anchor-Name Float-Complex) (Immutable-HashTable Geo-Anchor-Name (Option (GLayerof Dia:Block)))
                                     (Listof (GLayerof Geo-Path)) (Option Nonnegative-Flonum)
                                     (Dia-Zone-Identifier Dia-Zone-Style) (Dia-Zone-Typesetter Dia-Zone-Style) (Dia-Zone-Builder Dia-Zone-Style)
                                     Dia-Zone-Backstop-Style (Option Dia-Zone-Describer)
                                     (Option (GLayerof Geo)))
  (lambda [self positions blockdb tracks opacity identify typeset build backstop-style zone-desc]
    #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-anchor-pin? : (-> Geo-Anchor-Name Boolean)
  (lambda [anchor]
    (string-prefix? (geo-anchor->string anchor) ".")))

(define dia-anchor->boundary : (-> Geo-Anchor-Name (Immutable-HashTable Geo-Anchor-Name (Option (GLayerof Dia:Block))) (HashTable Geo-Anchor-Name Float-Complex)
                                   (Option (Immutable-Vector Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum)))
  (lambda [anchor blockdb positions]
    (define block (hash-ref blockdb anchor λfalse))

    (if (not block)
        (let ([pos (hash-ref positions anchor λfalse)])
          (and pos
               (vector-immutable (real-part pos) (imag-part pos) 0.0 0.0)))
        (let-values ([(x y) (geo-layer-position-values block)]
                     [(w h) (geo-layer-size block)])
          (vector-immutable x y w h)))))

(define dia-zone-realize : (-> Geo:Track:Zone Nonnegative-Flonum Nonnegative-Flonum Float-Complex (Option Nonnegative-Flonum)
                               (Dia-Zone-Identifier Dia-Zone-Style) (Dia-Zone-Typesetter Dia-Zone-Style) (Dia-Zone-Builder Dia-Zone-Style)
                               Dia-Zone-Backstop-Style (Option Dia-Zone-Describer) Geo-Insets-Mask
                               (-> (#%Dia-Zone-Style Dia-Zone-Style)) (-> (Option (Dia-Zone-Theme-Adjuster Dia-Zone-Style Dia-Zone-Metadata)))
                               (Option (GLayerof Geo)))
  (lambda [self width height position opacity zone-identify mk-title mk-zone backstop-style zone-desc mask make-style default-adjuster]
    (define-values (id type desc) (values (geo:track:zone-id self) (geo:track:zone-type self) (geo:track:zone-desc self)))
    (define-values (text stereotype) (dia-identity-extract id))
    (define initial-style (zone-identify id type stereotype))

    (and initial-style
         (let* ([style (if (void? initial-style) (make-style) initial-style)]
                [style ((inst dia-zone-theme-adjust Dia-Zone-Style Dia-Zone-Metadata) style id type (default-adjuster) stereotype)])
           (and style
                (let ([style-spec ((inst make-dia-zone-style-spec Dia-Zone-Style) #:custom style #:backstop backstop-style #:opacity opacity)])
                  (parameterize ([default-font-metrics (λ [[unit : Font-Unit]] (font-metrics-ref (dia-zone-resolve-font style-spec) unit))])
                    (let* ([maybe-title (cond [(not text) desc]
                                              [(not zone-desc) (void)]
                                              [(hash? zone-desc) (hash-ref zone-desc id void)]
                                              [else (zone-desc id type text style-spec stereotype)])]
                           [title (and maybe-title (mk-title id type (if (void? maybe-title) (or desc text) maybe-title) style-spec width height))]
                           [zone (mk-zone id type title style-spec width height stereotype mask)])
                      (cond [(pair? zone) (geo-sticker->layer #:default-anchor 'lt (car zone) position (cdr zone))]
                            [else #false])))))))))

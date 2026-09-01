#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require digimon/measure)
(require digimon/function)

(require geofun/font)
(require geofun/digitama/self)

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
        (Listof (GLayerof Geo)))
  (lambda [self positions blockdb tracks opacity identify typeset build backstop-style zone-desc]
    (define-values (style name stereotype)
      (dia-zone-resolve-style self identify backstop-style opacity
                              default-dia-rubber-zone-style
                              default-dia-rubber-zone-theme-adjuster))

    (if (and style)
        (let ([zone-font (dia-zone-resolve-font style)])
          (parameterize ([default-font-metrics (λ [[unit : Font-Unit]] (font-metrics-ref zone-font unit))])
            (define-values (children sublx subty subrx subby)
              (let realize : (Values (Listof (GLayerof Geo)) Flonum Flonum Flonum Flonum)
                ([children : (Listof Geo:Track:Zone:Rubber) (geo:track:zone:rubber-children self)]
                 [subzones : (Listof (GLayerof Geo)) null]
                 [lx : Flonum +inf.0] 
                 [ty : Flonum +inf.0] 
                 [rx : Flonum -inf.0] 
                 [by : Flonum -inf.0])
                (if (pair? children)
                    (let ([rest (cdr children)]
                          [subselves (dia-rubber-zone-realize (car children) positions blockdb tracks opacity
                                                              identify typeset build backstop-style zone-desc)])
                      (if (pair? subselves)
                          (let*-values ([(master) (car subselves)]
                                        [(x y) (values (glayer-x master) (glayer-y master))]
                                        [(x+w y+h) (values (+ x (glayer-width master)) (+ y (glayer-height master)))])
                            (realize rest (append subzones subselves)
                                     (min x lx) (min y ty) (max rx x+w) (max by y+h)))
                          (realize rest subzones lx ty rx by)))
                    (values subzones lx ty rx by))))
            
            (define maybe-title (dia-zone-title self name stereotype style typeset zone-desc))
            
            (let resolve-zone-boundary : (Listof (GLayerof Geo)) ([lx : Flonum sublx] [lfixed? : Boolean #false]
                                                                  [ty : Flonum subty] [tfixed? : Boolean #false]
                                                                  [rx : Flonum subrx] [rfixed? : Boolean #false]
                                                                  [by : Flonum subby] [bfixed? : Boolean #false]
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
                    (or (and (>= width 0.0) (>= height 0.0)
                             (let ([master (dia-zone-realize self maybe-title stereotype style width height (make-rectangular lx ty) build
                                                             (vector-immutable (not tfixed?) (not rfixed?) (not bfixed?) (not lfixed?)))])
                               (and master (cons master children))))
                        null))))))
        null)))
    
(define dia-fixed-zone-realize
  : (-> Geo:Track:Zone:Fixed (HashTable Geo-Anchor-Name Float-Complex) (Immutable-HashTable Geo-Anchor-Name (Option (GLayerof Dia:Block)))
        (Listof (GLayerof Geo-Path)) (Option Nonnegative-Flonum)
        (Dia-Zone-Identifier Dia-Zone-Style) (Dia-Zone-Typesetter Dia-Zone-Style) (Dia-Zone-Builder Dia-Zone-Style)
        Dia-Zone-Backstop-Style (Option Dia-Zone-Describer)
        (Listof (GLayerof Geo)))
  (lambda [self positions blockdb tracks opacity identify typeset build backstop-style zone-desc]
    null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-anchor-pin? : (-> Geo-Anchor-Name Boolean)
  (lambda [anchor]
    (define name (geo-anchor->string anchor))
    (define size (string-length name))

    (and (>= size 2)
         (eq? (string-ref name 0) #\.)
         (eq? (string-ref name (sub1 size)) #\.))))

(define dia-anchor->boundary
  : (-> Geo-Anchor-Name (Immutable-HashTable Geo-Anchor-Name (Option (GLayerof Dia:Block))) (HashTable Geo-Anchor-Name Float-Complex)
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

(define dia-zone-resolve-style : (-> Geo:Track:Zone (Dia-Zone-Identifier Dia-Zone-Style) Dia-Zone-Backstop-Style (Option Nonnegative-Flonum)
                                     (-> (#%Dia-Zone-Style Dia-Zone-Style)) (-> (Option Dia-Zone-Theme-Adjuster))
                                     (Values (Option (Dia-Zone-Style-Spec Dia-Zone-Style)) String Dia-Zone-Metadata))
  (lambda [self zone-identify backstop-style opacity make-style default-adjuster]
    (define-values (id type) (values (geo:track:zone-id self) (geo:track:zone-type self)))
    (define-values (name stereotype) (dia-identity-extract id))
    (define options : Dia-Zone-Metadata (cons stereotype (geo:track:zone-options self)))

    (values (and (> (string-length name) 0)
                 (not (eq? (string-ref name 0) #\.))
                 (let ([initial-style (zone-identify id type options)])
                   (and initial-style
                        (let* ([style (if (void? initial-style) (make-style) initial-style)]
                               [style ((inst dia-zone-theme-adjust Dia-Zone-Style Dia-Zone-Metadata) style id type (default-adjuster) options)])
                          (and style ((inst make-dia-zone-style-spec Dia-Zone-Style) #:custom style #:backstop backstop-style #:opacity opacity))))))
            name options)))

(define dia-zone-title : (-> Geo:Track:Zone String Dia-Zone-Metadata (Dia-Zone-Style-Spec Dia-Zone-Style)
                             (Dia-Zone-Typesetter Dia-Zone-Style) (Option Dia-Zone-Describer)
                             (Option Geo))
  (lambda [self name stereotype style mk-title zone-desc]
    (define-values (id type desc) (values (geo:track:zone-id self) (geo:track:zone-type self) (geo:track:zone-desc self)))
    (define maybe-title
      (cond [(hash? zone-desc) (hash-ref zone-desc id void)]
            [(and zone-desc) (zone-desc id type name style stereotype)]))
    
    (and maybe-title
         (mk-title id type (if (void? maybe-title) (or desc name) maybe-title) style))))
    
(define dia-zone-realize : (-> Geo:Track:Zone (Option Geo) Dia-Zone-Metadata (Dia-Zone-Style-Spec Dia-Zone-Style)
                               Nonnegative-Flonum Nonnegative-Flonum Float-Complex (Dia-Zone-Builder Dia-Zone-Style) Geo-Insets-Mask
                               (Option (GLayerof Geo)))
  (lambda [self title stereotype style width height position mk-zone mask]
    (define-values (id type) (values (geo:track:zone-id self) (geo:track:zone-type self)))
    (define zone (mk-zone id type title style width height stereotype mask))

    (cond [(pair? zone) (geo-sticker->layer #:default-anchor 'lt (car zone) position (cdr zone))]
          [else #false])))

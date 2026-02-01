#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require digimon/measure)
(require geofun/font)

(require geofun/digitama/self)
(require geofun/digitama/layer/type)
(require geofun/digitama/richtext/self)
(require geofun/digitama/geometry/footprint)

(require geofun/digitama/dc/track)
(require geofun/digitama/dc/path)
(require geofun/digitama/track/trail)
(require geofun/digitama/track/anchor)

(require "style.rkt")
(require "refine.rkt")
(require "label.rkt")
(require "backstop.rkt")
(require "interface.rkt")

(require "../block/dc.rkt")
(require "../block/style.rkt")
(require "../block/realize.rkt")
(require "../block/interface.rkt")
(require "../block/backstop.rkt")

(require "../decoration/interface.rkt")
(require "../decoration/backstop.rkt")
(require "../decoration/note.rkt")
(require "../decoration/freetrack.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (TS BS BM) dia-track-realize : (-> Geo:Track
                                                    (Dia-Track-Factory TS) (Option Dia-Free-Track-Factory)
                                                    (Dia-Block-Factory BS BM) (Option (Dia-Block-Describer BS BM))
                                                    (Option Dia-Note-Factory) (Option Dia-Note-Describer)
                                                    Real (Option Nonnegative-Real) (Option Geo-Rich-Text)
                                                    (Values (Listof (GLayerof Geo)) (Listof (GLayerof Geo))))
  (lambda [self track-factory free-factory block-factory block-desc0 note-factory note-desc scale opacity0 home-name]
    (define gpath : Geo-Trail (geo:track-trail self))
    (define anchor-base : (Immutable-HashTable Float-Complex Geo-Anchor-Name) (geo-trail-anchored-positions gpath))
    (define block-scale (and (> scale 0.0) (not (= scale 1.0)) (real->double-flonum scale)))
    (define opacity (and opacity0 (< opacity0 1.0) (real->double-flonum opacity0)))

    (define block-desc (dia-register-home-name gpath home-name block-desc0))
    (define infobase (geo:track-foot-infos self))
    
    (define track-identifier (dia-track-factory-identifier track-factory))
    (define track-rootstyle (dia-track-factory-λroot-style track-factory))
    (define track-backstyle ((dia-track-factory-λbackstop-style track-factory)))
    (define track->path (dia-track-builder-compose (dia-track-factory-builder track-factory)))
    (define make-labels (dia-track-annotator-compose (dia-track-factory-annotator track-factory)))

    (define block-identifier (dia-block-factory-identifier block-factory))
    (define block-rootstyle (dia-block-factory-λroot-style block-factory))
    (define block-backstyle ((dia-block-factory-λbackstop-style block-factory)))
    (define make-block (dia-block-builder-compose (dia-block-factory-builder block-factory) (dia-block-factory-fallback-builder block-factory)))
    (define make-caption (dia-block-typesetter-compose (dia-block-factory-typesetter block-factory)))

    (define free-style0 (make-dia-free-track-style))
    (define free-adjuster (and free-factory (dia-free-track-factory-adjuster free-factory)))
    (define make-free-path (dia-free-track-builder-compose (and free-factory (dia-free-track-factory-builder free-factory))))
    (define make-free-labels (dia-track-annotator-compose (and free-factory (dia-free-track-factory-annotator free-factory))))
    (define free-backstyle (or (and free-factory (let ([f (dia-free-track-factory-λbackstop-style free-factory)]) (and f (f)))) track-backstyle))

    ; NOTICE: the footprints are initially reversed
    (let stick ([tracks : (Listof (GLayerof Geo)) null]
                [blocks : (HashTable Geo-Anchor-Name (Option (GLayerof Dia:Block))) (hasheq)]
                [prints : Geo-Path-Prints null]
                [target : (Option (GLayerof Dia:Block)) #false]
                [last-pt : (Option Float-Complex) #false]
                [stnirp : Geo-Path-Prints (geo:track-footprints self)])
      (if (pair? stnirp)

          (let*-values ([(self rest) (values (car stnirp) (cdr stnirp))])
            (define next-pt : (Option Float-Complex) (gpp-position self last-pt))
            (define anchor : (Option Geo-Anchor-Name) (and next-pt (hash-ref anchor-base next-pt (λ [] #false))))
            (define prints++ : (Pairof GPath:Datum Geo-Path-Prints) (cons self prints))

            (define-values (source blocks++)
              (cond [(or (not anchor) (not next-pt)) (values #false blocks)]
                    [(hash-has-key? blocks anchor) (values (hash-ref blocks anchor) blocks)]
                    [else (let* ([direction (and last-pt (angle (- last-pt next-pt)))]
                                 [new-block (dia-block-layer-realize block-identifier make-block make-caption block-rootstyle block-backstyle block-desc
                                                                     note-factory note-desc anchor next-pt direction block-scale opacity)])
                            (values new-block (hash-set blocks anchor new-block)))]))
            
            (cond [(glayer? source)
                   (cond [(eq? (gpath:datum-cmd self) #\M)
                          (stick (dia-track-cons source target prints++ tracks infobase
                                                 track-identifier make-labels track->path track-rootstyle track-backstyle opacity)
                                 blocks++ null #false next-pt rest)]
                         [(and target)
                          (stick (dia-track-cons source target prints++ tracks infobase
                                                 track-identifier make-labels track->path track-rootstyle track-backstyle opacity)
                                 blocks++ (list self) source next-pt rest)]
                         [(pair? tracks)
                          (stick (dia-track-cons source #false prints++ tracks infobase
                                                 track-identifier make-labels track->path track-rootstyle track-backstyle opacity)
                                 blocks++ (list self) source next-pt rest)]
                         [else (stick tracks blocks++ prints++ source next-pt rest)])]
                   
                  [(eq? (gpath:datum-cmd self) #\M)
                   (cond [(not free-factory)(stick tracks blocks null #false next-pt rest)]
                         [else (stick (dia-free-track-cons anchor-base prints++ tracks infobase
                                                           free-adjuster make-free-labels make-free-path
                                                           free-style0 free-backstyle opacity)
                                      blocks++ null #false next-pt rest)])]

                  [else (stick tracks blocks prints++ target next-pt rest)]))

          (values
           (let sort : (Listof (GLayerof Geo)) ([ordered-blocks : (Listof (GLayerof Geo)) null]
                                                [srohcna : (Listof Geo-Anchor-Name) (geo-trail-ranchors gpath)])
             (if (pair? srohcna)
                 (let ([block (hash-ref blocks (car srohcna) (λ [] #false))])
                   (sort (if (not block) ordered-blocks (cons block ordered-blocks)) (cdr srohcna)))
                 ordered-blocks))
           
           ;;; NOTE
           ; We need to draw tracks backwards,
           ;   as we usually create archtectural path before jumping back for various branches.
           ;   So that drawing backwards allow main loop path hiding branch paths sharing same routes.
           (reverse tracks))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-track-cons : (-> (GLayerof Dia:Block) (Option (GLayerof Dia:Block)) Geo-Path-Prints (Listof (GLayerof Geo)) Geo-Track-Infobase
                                          (Dia-Track-Identifier S) (Dia-Track-Annotator S) (Dia-Track-Builder S)
                                          (Option (Dia-Track-Link-Root-Style S)) Dia-Track-Backstop-Style (Option Nonnegative-Flonum)
                                          (Listof (GLayerof Geo)))
  (lambda [src-layer tgt-layer prints tracks infobase track-identify make-label make-path rootstyle backstyle opacity]
    (define ctracks : Geo-Path-Clean-Prints (gpp-cleanse prints))

    (or
     (and (pair? ctracks)
          (pair? (cdr ctracks))
          
          (let ([source (glayer-master src-layer)]
                [target (and tgt-layer (glayer-master tgt-layer))])
            (define retracks : (Option (Pairof Geo-Path-Clean-Prints* (Pairof Index Index)))
              (if (null? (cddr ctracks))
                  (dia-two-tracks-relocate-endpoints src-layer tgt-layer ctracks)
                  (dia-more-tracks-relocate-endpoints src-layer tgt-layer ctracks)))

            (and retracks
                 (let-values ([(label-text label-sofni extra-track-info) (dia-track-label-info-filter infobase ctracks (cadr retracks) (cddr retracks))])
                   (define style-self : (Option (Dia-Track-Style S)) (track-identify source target label-text extra-track-info))
            
                   (and style-self
                        (let* ([parent-style (and rootstyle (rootstyle style-self))]
                               [style-spec ((inst make-dia-track-style-spec S) #:custom style-self #:root parent-style #:backstop backstyle #:opacity opacity)])
                          (parameterize ([default-font-metrics (λ [[unit : Font-Unit]] (font-metrics-ref (dia-track-resolve-font style-spec) unit))])
                            (let ([labels (dia-track-label-info->label make-label style-spec label-sofni)]
                                  [path (make-path source target (car retracks) style-spec)])
                              (and (geo? path)
                                   (cons (geo-path-self-pin-layer (geo-path-attach-label path labels))
                                         tracks))))))))))
     tracks)))

(define #:forall (S) dia-free-track-cons : (-> (Immutable-HashTable Float-Complex Geo-Anchor-Name) Geo-Path-Prints (Listof (GLayerof Geo)) Geo-Track-Infobase
                                               (Option (Dia-Free-Track-Adjuster S)) (Dia-Free-Track-Annotator S) (Dia-Free-Track-Builder S)
                                               (Dia-Track-Style S) Dia-Track-Backstop-Style (Option Nonnegative-Flonum)
                                               (Listof (GLayerof Geo)))
  (lambda [anchorbase prints tracks infobase free-adjuster make-label make-path style0 backstyle opacity]
    (define ctracks : Geo-Path-Clean-Prints (gpp-cleanse prints))
    
    (or
     (and (pair? ctracks)
          (pair? (cdr ctracks))
          
          (let* ([src-endpt (gpp-clean-position (car ctracks))]
                 [tgt-endpt (gpp-clean-position (last ctracks))]
                 [source (hash-ref anchorbase src-endpt (λ [] src-endpt))]
                 [target (hash-ref anchorbase tgt-endpt (λ [] tgt-endpt))])
            (define-values (label-text label-sofni extra-track-info) (dia-track-label-info-filter infobase ctracks 0 0))
            (define style-self : (U (Dia-Track-Style S) Void False)
              (cond [(not free-adjuster) style0]
                    [else (free-adjuster style0 source target ctracks label-text extra-track-info)]))
            
            (and style-self
                 (let ([style-spec ((inst make-dia-track-style-spec S) #:custom (if (void? style-self) style0 style-self) #:backstop backstyle #:opacity opacity)])
                   (parameterize ([default-font-metrics (λ [[unit : Font-Unit]] (font-metrics-ref (dia-track-resolve-font style-spec) unit))])
                     (let ([labels (dia-track-label-info->label make-label style-spec label-sofni)]
                           [path (make-path source target ctracks style-spec)])
                       (and (geo? path)
                            (cons (geo-path-self-pin-layer (geo-path-attach-label path labels)) tracks))))))))
  
     tracks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S M) dia-register-home-name : (-> Geo-Trail (Option Geo-Rich-Text) (Option (Dia-Block-Describer S M))
                                                    (Option (Dia-Block-Describer S M)))
  (lambda [self name block-desc]
    (define home (geo-trail-home-anchor self))
    
    (cond [(not name) block-desc]
          [(not block-desc) (make-immutable-hash (list (cons home name)))]
          [(hash? block-desc)
           (cond [(hash-has-key? block-desc home) block-desc]
                 [else (hash-set block-desc home name)])]
          [else (λ [[anchor : Geo-Anchor-Name] [text : String] [style : (Dia-Block-Style-Spec S)] [metadata : M]]
                  (define maybe (block-desc anchor text style metadata))
                  (cond [(not (eq? home anchor)) maybe]
                        [(void? maybe) name]
                        [else maybe]))])))

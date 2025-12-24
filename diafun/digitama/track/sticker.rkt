#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require digimon/digitama/unsafe/ops)

(require geofun/digitama/self)
(require geofun/digitama/markup)
(require geofun/digitama/geometry/trail)
(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/footprint)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/position)

(require geofun/digitama/dc/track)
(require geofun/digitama/track/self)
(require geofun/digitama/track/mult)

(require geofun/digitama/dc/path)
(require geofun/digitama/path/label)

(require "style.rkt")
(require "refine.rkt")

(require "../block/dc.rkt")
(require "../block/style.rkt")
(require "../interface.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Track-Label-Info (List Index Geo-Path-Labels Nonnegative-Flonum))

(define-syntax (dia-label-info->label stx)
  (syntax-case stx []
    [(_ source target edge-style make-label rinfos)
     (syntax/loc stx
       (let info->label ([labels : (Listof Geo:Path:Label) null]
                         [sofni : (Listof Dia-Track-Label-Info) rinfos])
         (if (pair? sofni)
             (let ([label (apply make-label source target edge-style (car sofni))])
               (info->label (dia-labels-cons label labels) (cdr sofni)))
             labels)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-track-stick
  (lambda [[master : Geo:Track]
           [block-identify : Dia-Block-Identifier] [make-block : (Option Dia-Anchor->Block)]
           [anchor->brief : Dia-Anchor->Brief] [block-desc : (Option Dia-Block-Describe)]
           [track-identify : Dia-Track-Identifier] [make-track : Dia-Track->Path] [make-track-label : Dia-Track->Label]
           [make-free-track : Dia-Free-Track->Path] [make-free-label : Dia-Free-Track->Label]
           [make-free-style : (Option (Dia-Track-Style-Make* Dia-Free-Track-Endpoint Dia-Free-Track-Endpoint (∩ Dia-Track-Style S)))]
           [fallback-block : Dia-Anchor->Block] [fallback-free-style : (-> (∩ Dia-Track-Style S))]
           [infobase : Geo-Track-Infobase] [ignore : (Listof Symbol)]] : (Values (Listof (GLayerof Geo)) (Listof (GLayerof Geo)))
    (define gpath : Geo-Trail (geo:track-trail master))
    (define anchor-base : (Immutable-HashTable Float-Complex Geo-Anchor-Name) (geo-trail-anchored-positions gpath))
    (define-values (Width Height) (geo-flsize master))

    ; NOTICE: the footprints are initially reversed
    (let stick ([stnirp : Geo-Path-Prints (geo:track-footprints master)]
                [tracks : (Listof (GLayerof Geo)) null]
                [blocks : (HashTable Geo-Anchor-Name (Option (GLayerof Dia:Block))) (hasheq)]
                [prints : Geo-Path-Prints null]
                [target : (Option (GLayerof Dia:Block)) #false]
                [last-pt : (Option Float-Complex) #false])
      (if (pair? stnirp)

          (let*-values ([(self rest) (values (car stnirp) (cdr stnirp))])
            (define next-pt : (Option Float-Complex) (gpp-position self last-pt))
            (define anchor : (Option Geo-Anchor-Name) (and next-pt (hash-ref anchor-base next-pt (λ [] #false))))
            (define prints++ : (Pairof GPath:Datum Geo-Path-Prints) (cons self prints))

            (define-values (source blocks++)
              (cond [(or (not anchor) (not next-pt)) (values #false blocks)]
                    [(hash-has-key? blocks anchor) (values (hash-ref blocks anchor) blocks)]
                    [else (let* ([direction (and last-pt (angle (- last-pt next-pt)))]
                                 [new-block (dia-block-layer-make block-identify make-block anchor->brief block-desc
                                                                  fallback-block anchor next-pt direction ignore)])
                            (values new-block (hash-set blocks anchor new-block)))]))
            
            (cond [(glayer? source)
                   (cond [(eq? (gpath:datum-cmd self) #\M)
                          (let ([tracks++ (dia-track-cons source target prints++ track-identify make-track tracks make-track-label infobase)])
                            (stick rest tracks++ blocks++ null #false next-pt))]
                         [(and target)
                          (let ([tracks++ (dia-track-cons source target prints++ track-identify make-track tracks make-track-label infobase)])
                            (stick rest tracks++ blocks++ (list self) source next-pt))]
                         [(pair? tracks)
                          (let ([tracks++ (dia-track-cons source #false prints++ track-identify make-track tracks make-track-label infobase)])
                            (stick rest tracks++ blocks++ (list self) source next-pt))]
                         [else (stick rest tracks blocks++ prints++ source next-pt)])]
                   
                  [(eq? (gpath:datum-cmd self) #\M)
                   (let ([ct (gpp-cleanse prints++)])
                     (if (and (pair? ct) (pair? (cdr ct)))
                         (let ([tracks++ (dia-free-track-cons anchor-base ct make-free-track tracks make-free-label infobase
                                                              make-free-style fallback-free-style)])
                           (stick rest tracks++ blocks++ null #false next-pt))
                         (stick rest tracks blocks null #false next-pt)))]

                  [else (stick rest tracks blocks prints++ target next-pt)]))

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
(define dia-track-cons : (-> (GLayerof Dia:Block) (Option (GLayerof Dia:Block)) Geo-Path-Prints
                             Dia-Track-Identifier Dia-Track->Path (Listof (GLayerof Geo))
                             Dia-Track->Label Geo-Track-Infobase
                             (Listof (GLayerof Geo)))
  (lambda [src-layer tgt-layer prints track-identify make-track tracks make-label infobase]
    (define ctracks : Geo-Path-Clean-Prints (gpp-cleanse prints))

    (or (and (pair? ctracks)
             (pair? (cdr ctracks))

             (let ([source (glayer-master src-layer)]
                   [target (and tgt-layer (glayer-master tgt-layer))])
               (define retracks : Geo-Path-Clean-Prints*
                 (if (null? (cddr ctracks))
                     (dia-2-tracks-relocate-endpoints src-layer tgt-layer ctracks)
                     (dia-more-tracks-relocate-endpoints src-layer tgt-layer ctracks)))
               
               (define-values (labels sofni extra-info) (dia-track-label-info-filter infobase ctracks retracks))
               (define edge-style : (Option Dia-Track-Style) (track-identify source target labels extra-info))

               (define track : (U Geo:Path Void False)
                 (and edge-style
                      (make-track source target edge-style retracks
                                  (dia-label-info->label source target edge-style make-label sofni))))

               (and (geo? track) (cons (geo-path-self-pin-layer track) tracks))))
        
        tracks)))

(define dia-track-label-info-filter : (-> Geo-Track-Infobase Geo-Path-Clean-Prints Geo-Path-Clean-Prints
                                          (Values (Listof Geo-Path-Labels) (Listof Dia-Track-Label-Info)
                                                  (Listof Geo-Track-Info-Datum)))
  (lambda [infobase tracks refined-tracks]
    (let label-filter ([sofni : (Listof Dia-Track-Label-Info) null]
                       [slebal : (Listof Geo-Path-Labels) null]
                       [extra-infos : (Listof Geo-Track-Info-Datum) null]
                       [orig-tracks : Geo-Path-Clean-Prints tracks]
                       [refd-tracks : Geo-Path-Clean-Prints refined-tracks]
                       [orig-src : (Option Float-Complex) #false]
                       [idx : Index 0])
      (if (and (pair? orig-tracks) (pair? refd-tracks))

          (let ([oself (car orig-tracks)]
                [rself (car refd-tracks)])
            (cond [(eq? (gpath:datum-cmd oself) #\M)
                   (label-filter sofni slebal extra-infos (cdr orig-tracks) (cdr refd-tracks)
                                 (gpp-clean-position oself)
                                 idx)]
                  [(eq? (gpath:datum-cmd oself) #\L)
                   (let* ([otarget (gpp-clean-position oself)]
                          [rtarget (gpp-clean-position rself)]
                          [info (and orig-src (hash-ref infobase (cons orig-src otarget) (λ [] #false)))])
                     (define-values (maybe-label base-position maybe-mult extra++)
                       (if (geo:track:info? info)
                           (let ([labels (geo:track:info-labels info)]
                                 [t (geo:track:info-base-position info)]
                                 [mult (geo:track:info-multiplicity info)]
                                 [extra (geo:track:info-extra info)])
                             (if (null? extra)
                                 (values labels t mult extra-infos)
                                 (values labels t mult (append extra-infos extra))))
                           (values #false 0.0 #false extra-infos)))

                     (define-values (label-info slabel++)
                       (if (or maybe-label)
                           (values (list idx maybe-label base-position)
                                   (cons maybe-label slebal))
                           (values #false slebal)))

                     (define mult-info : (Option Dia-Track-Label-Info)
                       (let ([labels (and maybe-mult (geo-track-multiplicities-map (geo:track:multiplicity-source maybe-mult)
                                                                                   (geo:track:multiplicity-target maybe-mult)))])
                         (and labels
                              (list idx labels
                                    (geo:track:multiplicity-base-position maybe-mult)))))
                   
                     (label-filter (cond [(and label-info mult-info) (list* label-info mult-info sofni)]
                                         [(and label-info) (cons label-info sofni)]
                                         [(and mult-info) (cons mult-info sofni)]
                                         [else sofni])
                                   slabel++ extra++ (cdr orig-tracks) (cdr refd-tracks) otarget
                                   (unsafe-idx+ idx 1)))]

                  ;;; TODO: deal with curves
                  [else (label-filter sofni slebal extra-infos
                                      (cdr orig-tracks) (cdr refd-tracks) orig-src
                                      (unsafe-idx+ idx 1))]))
          
          (values (reverse slebal) sofni extra-infos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-free-track-cons : (-> (Immutable-HashTable Float-Complex Geo-Anchor-Name)
                                               Geo-Path-Clean-Prints* Dia-Free-Track->Path (Listof (GLayerof Geo))
                                               Dia-Free-Track->Label Geo-Track-Infobase
                                               (Option (Dia-Track-Style-Make* Dia-Free-Track-Endpoint Dia-Free-Track-Endpoint (∩ Dia-Track-Style S)))
                                               (-> (∩ Dia-Track-Style S))
                                               (Listof (GLayerof Geo)))
  (lambda [anchorbase prints make-edge tracks make-label infobase make-free-style fallback-free-style]
    (define src-endpt : Float-Complex (gpp-clean-position (car prints)))
    (define tgt-endpt : Float-Complex (gpp-clean-position (last prints)))
    (define source : Dia-Free-Track-Endpoint (hash-ref anchorbase src-endpt (λ [] src-endpt)))
    (define target : Dia-Free-Track-Endpoint (hash-ref anchorbase tgt-endpt (λ [] tgt-endpt)))
    (define-values (labels sofni extra-info) (dia-track-label-info-filter infobase prints prints))
    (define edge-style : Dia-Track-Style (dia-track-style-construct source target labels make-free-style fallback-free-style))

    (define track : (U Geo:Path Void False)
      (make-edge source target edge-style prints
                 (dia-label-info->label source target edge-style make-label sofni)))
    
    (if (geo? track) (cons (geo-path-self-pin-layer track) tracks) tracks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-block-make : (-> Dia-Block-Identifier (Option Dia-Anchor->Block) Dia-Anchor->Brief (Option Dia-Block-Describe)
                             Dia-Anchor->Block Geo-Anchor-Name (Option Flonum) (Listof Symbol)
                             (Option Dia:Block))
  (lambda [block-identify make-block make-brief block-desc fallback-block anchor direction ignore]
    (define blk-info (block-identify anchor))

    (and blk-info
         (let-values ([(id) (geo-anchor->symbol anchor)]
                      [(text style datum) (values (car blk-info) (cadr blk-info) (caddr blk-info))])
           (define maybe-desc : (U DC-Markup-Text Void False)
             (and block-desc
                  (if (hash? block-desc)
                      (hash-ref block-desc anchor (λ [] #false))
                      (block-desc anchor text))))
           
           (define brief : (Option Geo) (make-brief id (if (dc-markup-text? maybe-desc) maybe-desc text) style datum))
           (define-values (width height) (dia-block-smart-size brief style))
           (define block : (U Dia:Block Void False)
             (cond [(memq id ignore) #false]
                   [(not make-block) (void)]
                   [else (make-block id brief style width height direction datum)]))

           (if (void? block)
               (let ([fallback-block (fallback-block id brief style width height direction datum)])
                 (and (dia:block? fallback-block)
                      fallback-block))
               block)))))

(define dia-block-layer-make : (-> Dia-Block-Identifier (Option Dia-Anchor->Block) Dia-Anchor->Brief (Option Dia-Block-Describe)
                                   Dia-Anchor->Block Geo-Anchor-Name Float-Complex (Option Flonum) (Listof Symbol)
                                   (Option (GLayerof Dia:Block)))
  (lambda [block-identify make-block make-brief block-desc fallback-block anchor position direction ignore]
    (define maybe-block (dia-block-make block-identify make-block make-brief block-desc fallback-block anchor direction ignore))

    (and maybe-block
         (geo-own-pin-layer 'cc position maybe-block 0.0+0.0i))))

(define dia-labels-cons : (-> (U (Listof Geo:Path:Label) Geo:Path:Label Void False) (Listof Geo:Path:Label) (Listof Geo:Path:Label))
  (lambda [label alabels]
    (cond [(list? label) (append label alabels)]
          [(geo:path:label? label) (cons label alabels)]
          [else alabels])))

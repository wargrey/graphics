#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require digimon/digitama/unsafe/ops)

(require geofun/digitama/convert)
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

(require "interface.rkt")

(require "../node/dc.rkt")
(require "../node/style.rkt")
(require "../edge/style.rkt")
(require "../edge/refine.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Path-Arrow-Label-Info (List Index Geo-Path-Labels Nonnegative-Flonum))

(define-syntax (dia-label-info->label stx)
  (syntax-case stx []
    [(_ source target edge-style make-label rinfos)
     (syntax/loc stx
       (let info->label ([labels : (Listof Geo:Path:Label) null]
                         [sofni : (Listof Dia-Path-Arrow-Label-Info) rinfos])
         (if (pair? sofni)
             (let ([label (apply make-label source target edge-style (car sofni))])
               (info->label (dia-cons-labels label labels) (cdr sofni)))
             labels)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-path-stick
  (lambda [[master : Geo:Track]
           [block-identify : Dia-Path-Block-Identifier] [make-node : (Option Dia-Path-Id->Node-Shape)]
           [make-node-label : Dia-Path-Id->Node-Label] [node-desc : (Option Dia-Path-Id->Label-String)]
           [arrow-identify : Dia-Path-Arrow-Identifier] [make-arrow : Dia-Path-Arrow->Edge] [make-arrow-label : Dia-Path-Arrow->Edge-Label]
           [make-free-track : Dia-Path-Free-Track->Edge] [make-free-label : Dia-Path-Free-Track->Edge-Label]
           [make-free-style : (Option (Dia-Edge-Style-Make* Dia-Free-Edge-Endpoint Dia-Free-Edge-Endpoint (∩ Dia-Edge-Style S)))]
           [fallback-node : Dia-Path-Id->Node-Shape] [fallback-free-style : (-> (∩ Dia-Edge-Style S))]
           [infobase : Geo-Track-Infobase] [ignore : (Listof Symbol)]] : (Values (Listof (GLayerof Geo)) (Listof (GLayerof Geo)))
    (define gpath : Geo-Trail (geo:track-trail master))
    (define anchor-base : (Immutable-HashTable Float-Complex Geo-Anchor-Name) (geo-trail-anchored-positions gpath))
    (define-values (Width Height) (geo-flsize master))

    ; NOTICE: the footprints are initially reversed
    (let stick ([stnirp : Geo-Path-Prints (geo:track-footprints master)]
                [arrows : (Listof (GLayerof Geo)) null]
                [nodes : (HashTable Geo-Anchor-Name (Option (GLayerof Dia:Node))) (hasheq)]
                [tracks : Geo-Path-Prints null]
                [target : (Option (GLayerof Dia:Node)) #false]
                [last-pt : (Option Float-Complex) #false])
      (if (pair? stnirp)

          (let*-values ([(self rest) (values (car stnirp) (cdr stnirp))])
            (define next-pt : (Option Float-Complex) (gpp-position self last-pt))
            (define anchor : (Option Geo-Anchor-Name) (and next-pt (hash-ref anchor-base next-pt (λ [] #false))))
            (define tracks++ : (Pairof GPath:Datum Geo-Path-Prints) (cons self tracks))

            (define-values (source nodes++)
              (cond [(or (not anchor) (not next-pt)) (values #false nodes)]
                    [(hash-has-key? nodes anchor) (values (hash-ref nodes anchor) nodes)]
                    [else (let* ([direction (and last-pt (angle (- last-pt next-pt)))]
                                 [new-node (dia-make-node-layer block-identify make-node make-node-label node-desc
                                                                fallback-node anchor next-pt direction ignore)])
                            (values new-node (hash-set nodes anchor new-node)))]))
            
            (cond [(glayer? source)
                   (cond [(eq? (gpath:datum-cmd self) #\M)
                          (let ([arrows++ (dia-arrow-cons source target tracks++ arrow-identify make-arrow arrows make-arrow-label infobase)])
                            (stick rest arrows++ nodes++ null #false next-pt))]
                         [(and target)
                          (let ([arrows++ (dia-arrow-cons source target tracks++ arrow-identify make-arrow arrows make-arrow-label infobase)])
                            (stick rest arrows++ nodes++ (list self) source next-pt))]
                         [(pair? tracks)
                          (let ([arrows++ (dia-arrow-cons source #false tracks++ arrow-identify make-arrow arrows make-arrow-label infobase)])
                            (stick rest arrows++ nodes++ (list self) source next-pt))]
                         [else (stick rest arrows nodes++ tracks++ source next-pt)])]
                   
                   [(eq? (gpath:datum-cmd self) #\M)
                    (let ([ct (gpp-cleanse tracks++)])
                      (if (and (pair? ct) (pair? (cdr ct)))
                          (let ([arrows++ (dia-free-track-cons anchor-base ct make-free-track arrows make-free-label infobase
                                                               make-free-style fallback-free-style)])
                            (stick rest arrows++ nodes++ null #false next-pt))
                          (stick rest arrows nodes null #false next-pt)))]

                   [else (stick rest arrows nodes tracks++ target next-pt)]))

          (values
           (let sort : (Listof (GLayerof Geo)) ([ordered-nodes : (Listof (GLayerof Geo)) null]
                                                [srohcna : (Listof Geo-Anchor-Name) (geo-trail-ranchors gpath)])
             (if (pair? srohcna)
                 (let ([node (hash-ref nodes (car srohcna) (λ [] #false))])
                   (sort (if (not node) ordered-nodes (cons node ordered-nodes)) (cdr srohcna)))
                 ordered-nodes))
           
           ;;; NOTE
           ; We need to draw arrows backwards,
           ;   as we usually create archtectural path before jumping back for various branches.
           ;   So that drawing backwards allow main loop path hiding branch paths sharing same routes.
           (reverse arrows))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-arrow-cons : (-> (GLayerof Dia:Node) (Option (GLayerof Dia:Node)) Geo-Path-Prints
                             Dia-Path-Arrow-Identifier Dia-Path-Arrow->Edge (Listof (GLayerof Geo))
                             Dia-Path-Arrow->Edge-Label Geo-Track-Infobase
                             (Listof (GLayerof Geo)))
  (lambda [src-layer tgt-layer tracks arrow-identify make-arrow arrows make-label infobase]
    (define ctracks : Geo-Path-Clean-Prints (gpp-cleanse tracks))

    (or (and (pair? ctracks)
             (pair? (cdr ctracks))

             (let ([source (glayer-master src-layer)]
                   [target (and tgt-layer (glayer-master tgt-layer))])
               (define retracks : Geo-Path-Clean-Prints*
                 (if (null? (cddr ctracks))
                     (dia-2-tracks-relocate-endpoints src-layer tgt-layer ctracks)
                     (dia-more-tracks-relocate-endpoints src-layer tgt-layer ctracks)))
               
               (define-values (labels sofni extra-info) (dia-arrow-label-info-filter infobase ctracks retracks))
               (define edge-style : (Option Dia-Edge-Style) (arrow-identify source target labels extra-info))

               (define arrow : (U Geo:Path Void False)
                 (and edge-style
                      (make-arrow source target edge-style retracks
                                  (dia-label-info->label source target edge-style make-label sofni))))

               (and (geo? arrow) (cons (geo-path-self-pin-layer arrow) arrows))))
        
        arrows)))

(define dia-arrow-label-info-filter : (-> Geo-Track-Infobase Geo-Path-Clean-Prints Geo-Path-Clean-Prints
                                          (Values (Listof Geo-Path-Labels) (Listof Dia-Path-Arrow-Label-Info)
                                                  (Listof Geo-Track-Info-Datum)))
  (lambda [infobase tracks refined-tracks]
    (let label-filter ([sofni : (Listof Dia-Path-Arrow-Label-Info) null]
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

                     (define mult-info : (Option Dia-Path-Arrow-Label-Info)
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
                                               Geo-Path-Clean-Prints* Dia-Path-Free-Track->Edge (Listof (GLayerof Geo))
                                               Dia-Path-Free-Track->Edge-Label Geo-Track-Infobase
                                               (Option (Dia-Edge-Style-Make* Dia-Free-Edge-Endpoint Dia-Free-Edge-Endpoint (∩ Dia-Edge-Style S)))
                                               (-> (∩ Dia-Edge-Style S))
                                               (Listof (GLayerof Geo)))
  (lambda [anchorbase tracks make-edge arrows make-label infobase make-free-style fallback-free-style]
    (define src-endpt : Float-Complex (gpp-clean-position (car tracks)))
    (define tgt-endpt : Float-Complex (gpp-clean-position (last tracks)))
    (define source : Dia-Free-Edge-Endpoint (hash-ref anchorbase src-endpt (λ [] src-endpt)))
    (define target : Dia-Free-Edge-Endpoint (hash-ref anchorbase tgt-endpt (λ [] tgt-endpt)))
    (define-values (labels sofni extra-info) (dia-arrow-label-info-filter infobase tracks tracks))
    (define edge-style : Dia-Edge-Style (dia-edge-style-construct source target labels make-free-style fallback-free-style))

    (define arrow : (U Geo:Path Void False)
      (make-edge source target edge-style tracks
                 (dia-label-info->label source target edge-style make-label sofni)))
    
    (if (geo? arrow) (cons (geo-path-self-pin-layer arrow) arrows) arrows)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-make-node : (-> Dia-Path-Block-Identifier (Option Dia-Path-Id->Node-Shape) Dia-Path-Id->Node-Label (Option Dia-Path-Id->Label-String)
                            Dia-Path-Id->Node-Shape Geo-Anchor-Name (Option Flonum) (Listof Symbol)
                            (Option Dia:Node))
  (lambda [block-identify make-node make-node-label node-desc fallback-node anchor direction ignore]
    (define blk-datum (block-identify anchor))

    (and blk-datum
         (let-values ([(id) (geo-anchor->symbol anchor)]
                      [(style hint) (values (cadr blk-datum) (caddr blk-datum))])
           (define maybe-desc : (U String Void False)
             (and node-desc
                  (if (hash? node-desc)
                      (hash-ref node-desc anchor (λ [] #false))
                      (node-desc anchor (car blk-datum)))))
           
           (define label : (Option Geo) (make-node-label id (if (string? maybe-desc) maybe-desc (car blk-datum)) style hint))
           (define-values (width height) (dia-node-smart-size label style))
           (define node : (U Dia:Node Void False)
             (cond [(memq id ignore) #false]
                   [(not make-node) (void)]
                   [else (make-node id label style width height direction hint)]))

           (if (void? node)
               (let ([fallback-node (fallback-node id label style width height direction hint)])
                 (and (dia:node? fallback-node)
                      fallback-node))
               node)))))

(define dia-make-node-layer : (-> Dia-Path-Block-Identifier (Option Dia-Path-Id->Node-Shape) Dia-Path-Id->Node-Label (Option Dia-Path-Id->Label-String)
                                  Dia-Path-Id->Node-Shape Geo-Anchor-Name Float-Complex (Option Flonum) (Listof Symbol)
                                  (Option (GLayerof Dia:Node)))
  (lambda [block-identify make-node make-node-label node-desc fallback-node anchor position direction ignore]
    (define maybe-node (dia-make-node block-identify make-node make-node-label node-desc fallback-node anchor direction ignore))

    (and maybe-node
         (geo-own-pin-layer 'cc position maybe-node 0.0+0.0i))))

(define dia-cons-labels : (-> (U (Listof Geo:Path:Label) Geo:Path:Label Void False) (Listof Geo:Path:Label) (Listof Geo:Path:Label))
  (lambda [label alabels]
    (cond [(list? label) (append label alabels)]
          [(geo:path:label? label) (cons label alabels)]
          [else alabels])))

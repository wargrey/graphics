#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require geofun/digitama/convert)
(require geofun/digitama/geometry/trail)
(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/footprint)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/position)

(require geofun/digitama/dc/path)
(require geofun/digitama/path/self)

(require "interface.rkt")

(require "../node/dc.rkt")
(require "../node/style.rkt")
(require "../edge/style.rkt")
(require "../edge/label.rkt")
(require "../edge/refine.rkt")
(require "../edge/dc.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Path-Arrow-Label-Info (List Float-Complex Float-Complex Dia-Edge-Label-Datum Nonnegative-Flonum))

(define-syntax (dia-label-info->label stx)
  (syntax-case stx []
    [(_ source target edge-style make-label rinfos)
     (syntax/loc stx
       (let info->label ([labels : (Listof Dia-Edge-Label) null]
                         [sofni : (Listof Dia-Path-Arrow-Label-Info) rinfos])
         (if (pair? sofni)
             (let ([label (apply make-label source target edge-style (car sofni))])
               (info->label (dia-cons-labels label labels) (cdr sofni)))
             labels)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-path-stick
  (lambda [[master : Geo:Path]
           [block-identify : Dia-Path-Block-Identifier] [make-node : (Option Dia-Path-Id->Node-Shape)]
           [make-node-label : Dia-Path-Id->Node-Label] [node-desc : (Option Dia-Path-Id->Label-String)]
           [arrow-identify : Dia-Path-Arrow-Identifier] [make-arrow : Dia-Path-Arrow->Edge] [make-arrow-label : Dia-Path-Arrow->Edge-Label]
           [make-free-track : Dia-Path-Free-Track->Edge] [make-free-label : Dia-Path-Free-Track->Edge-Label]
           [make-free-style : (Option (Dia-Edge-Style-Make* Dia-Free-Edge-Endpoint Dia-Free-Edge-Endpoint (∩ Dia-Edge-Style S)))]
           [fallback-node : Dia-Path-Id->Node-Shape] [fallback-free-style : (-> (∩ Dia-Edge-Style S))]
           [infobase : Geo-Path-Infobase] [ignore : (Listof Symbol)]] : (Values (Listof (GLayerof Geo)) (Listof (GLayerof Geo)))
    (define gpath : Geo-Trail (geo:path-trail master))
    (define anchor-base : (Immutable-HashTable Float-Complex Geo-Anchor-Name) (geo-trail-anchored-positions gpath))
    (define-values (Width Height) (geo-flsize master))

    ; NOTICE: the footprints are initially reversed
    (let stick ([stnirp : Geo-Path-Prints (geo:path-footprints master)]
                [arrows : (Listof (GLayerof Geo)) null]
                [nodes : (HashTable Geo-Anchor-Name (Option (GLayerof Dia:Node))) (hasheq)]
                [tracks : Geo-Path-Prints null]
                [target : (Option (GLayerof Dia:Node)) #false]
                [last-pt : (Option Float-Complex) #false])
      (if (pair? stnirp)

          (let*-values ([(self rest) (values (car stnirp) (cdr stnirp))])
            (define next-pt : (Option Float-Complex) (geo-path-print-position self last-pt))
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
                    (let ([ct (geo-path-cleanse tracks++)])
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
                             Dia-Path-Arrow->Edge-Label Geo-Path-Infobase
                             (Listof (GLayerof Geo)))
  (lambda [src-layer tgt-layer tracks arrow-identify make-arrow arrows make-label infobase]
    (define ctracks : Geo-Path-Clean-Prints (geo-path-cleanse tracks))

    (or (and (pair? ctracks)
             (pair? (cdr ctracks))

             (let ([source (glayer-master src-layer)]
                   [target (and tgt-layer (glayer-master tgt-layer))])
               (define retracks : Geo-Path-Clean-Prints+
                 (if (null? (cddr ctracks))
                     (dia-2-tracks-relocate-endpoints src-layer tgt-layer ctracks)
                     (dia-more-tracks-relocate-endpoints src-layer tgt-layer ctracks)))
               
               (define-values (labels sofni extra-info) (dia-arrow-label-info-filter infobase ctracks retracks))
               (define edge-style : (Option Dia-Edge-Style) (arrow-identify source target labels extra-info))

               (define arrow : (U Dia:Edge Dia:Labeled-Edge Void False)
                 (and edge-style
                      (make-arrow source target edge-style retracks
                                  (dia-label-info->label source target edge-style make-label sofni))))

               (and (geo? arrow) (dia-cons-arrows arrow arrows))))
        
        arrows)))

(define dia-arrow-label-info-filter : (-> Geo-Path-Infobase Geo-Path-Clean-Prints Geo-Path-Clean-Prints
                                          (Values (Listof Dia-Edge-Label-Datum) (Listof Dia-Path-Arrow-Label-Info)
                                                  (Listof Geo-Path-Info-Datum)))
  (lambda [infobase tracks refined-tracks]
    (let label-filter ([sofni : (Listof Dia-Path-Arrow-Label-Info) null]
                       [slebal : (Listof Dia-Edge-Label-Datum) null]
                       [extra-infos : (Listof Geo-Path-Info-Datum) null]
                       [orig-tracks : Geo-Path-Clean-Prints tracks]
                       [refd-tracks : Geo-Path-Clean-Prints refined-tracks]
                       [orig-src : (Option Float-Complex) #false]
                       [refd-src : Float-Complex 0.0+0.0i])
      (if (and (pair? orig-tracks) (pair? refd-tracks))

          (let ([oself (car orig-tracks)]
                [rself (car refd-tracks)])
            (cond [(eq? (gpath:datum-cmd oself) #\M)
                   (label-filter sofni slebal extra-infos (cdr orig-tracks) (cdr refd-tracks)
                                 (geo-path-clean-print-position oself)
                                 (geo-path-clean-print-position rself))]
                  [(eq? (gpath:datum-cmd oself) #\L)
                   (let* ([otarget (geo-path-clean-print-position oself)]
                          [rtarget (geo-path-clean-print-position rself)]
                          [info (and orig-src (hash-ref infobase (cons orig-src otarget) (λ [] #false)))])
                     (define-values (maybe-label base-position maybe-mult extra++)
                       (if (geo:path:info? info)
                           (let ([labels (geo:path:info-labels info)]
                                 [t (geo:path:info-base-position info)]
                                 [mult (geo:path:info-multiplicity info)]
                                 [extra (geo:path:info-extra info)])
                             (if (null? extra)
                                 (values (dia-edge-label-map labels) t mult extra-infos)
                                 (values (dia-edge-label-map labels) t mult (append extra-infos extra))))
                           (values #false 0.0 #false extra-infos)))

                     (define-values (label-info slabel++)
                       (if (or maybe-label)
                           (values (list refd-src rtarget maybe-label base-position)
                                   (cons maybe-label slebal))
                           (values #false slebal)))

                     (define mult-info : (Option Dia-Path-Arrow-Label-Info)
                       (let ([labels (and maybe-mult (dia-edge-multiplicities-map (geo:path:multiplicity-source maybe-mult)
                                                                                  (geo:path:multiplicity-target maybe-mult)))])
                         (and labels
                              (list refd-src rtarget labels
                                    (geo:path:multiplicity-base-position maybe-mult)))))
                   
                     (label-filter (cond [(and label-info mult-info) (list* label-info mult-info sofni)]
                                         [(and label-info) (cons label-info sofni)]
                                         [(and mult-info) (cons mult-info sofni)]
                                         [else sofni])
                                   slabel++ extra++ (cdr orig-tracks) (cdr refd-tracks) otarget rtarget))]

                  ;;; TODO: deal with curves
                  [else (label-filter sofni slebal extra-infos (cdr orig-tracks) (cdr refd-tracks) orig-src refd-src)]))
          
          (values (reverse slebal) sofni extra-infos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-free-track-cons : (-> (Immutable-HashTable Float-Complex Geo-Anchor-Name)
                                               Geo-Path-Clean-Prints+ Dia-Path-Free-Track->Edge (Listof (GLayerof Geo))
                                               Dia-Path-Free-Track->Edge-Label Geo-Path-Infobase
                                               (Option (Dia-Edge-Style-Make* Dia-Free-Edge-Endpoint Dia-Free-Edge-Endpoint (∩ Dia-Edge-Style S)))
                                               (-> (∩ Dia-Edge-Style S))
                                               (Listof (GLayerof Geo)))
  (lambda [anchorbase tracks make-edge arrows make-label infobase make-free-style fallback-free-style]
    (define src-endpt : Float-Complex (geo-path-clean-print-position (car tracks)))
    (define tgt-endpt : Float-Complex (geo-path-clean-print-position (last tracks)))
    (define source : Dia-Free-Edge-Endpoint (hash-ref anchorbase src-endpt (λ [] src-endpt)))
    (define target : Dia-Free-Edge-Endpoint (hash-ref anchorbase tgt-endpt (λ [] tgt-endpt)))
    (define-values (labels sofni extra-info) (dia-arrow-label-info-filter infobase tracks tracks))
    (define edge-style : Dia-Edge-Style (dia-edge-style-construct source target labels make-free-style fallback-free-style))

    (define arrow : (U Dia:Edge Dia:Labeled-Edge Void False)
      (make-edge source target edge-style tracks
                 (dia-label-info->label source target edge-style make-label sofni)))
    
    (if (geo? arrow) (dia-cons-arrows arrow arrows) arrows)))

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

(define dia-cons-arrows : (-> (U Dia:Edge Dia:Labeled-Edge) (Listof (GLayerof Geo)) (Listof (GLayerof Geo)))
  (lambda [arrow arrows]
    (define ppos : Float-Complex (dia-edge-self-pin-position arrow #false))
    (define-values (awidth aheight) (geo-flsize arrow))
    (define alayer : (GLayerof Geo) (glayer arrow (real-part ppos) (imag-part ppos) awidth aheight))

    (cons alayer arrows)))

(define dia-cons-labels : (-> (U (Listof Dia-Edge-Label) Dia-Edge-Label Void False) (Listof Dia-Edge-Label) (Listof Dia-Edge-Label))
  (lambda [label alabels]
    (cond [(list? label) (append label alabels)]
          [(dia-edge-label? label) (cons label alabels)]
          [else alabels])))

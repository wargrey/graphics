#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/case)

(require geofun/digitama/convert)
(require geofun/digitama/geometry/trail)
(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/footprint)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/position)
(require geofun/digitama/dc/path)

(require "style.rkt")
(require "node.rkt")
(require "interface.rkt")

(require "../node/dc.rkt")
(require "../edge/style.rkt")
(require "../edge/label.rkt")
(require "../edge/refine.rkt")
(require "../edge/dc.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DiaFlow-Arrow-Label-Info (List Float-Complex Float-Complex Dia-Edge-Label-Datum))

(define-syntax (dia-label-info->label stx)
  (syntax-case stx []
    [(_ source target edge-style make-label rinfos)
     (syntax/loc stx
       (let info->label ([labels : (Listof Dia-Edge-Label) null]
                         [sofni : (Listof DiaFlow-Arrow-Label-Info) rinfos])
         (if (pair? sofni)
             (let ([label (apply make-label source target edge-style (car sofni))])
               (info->label (dia-cons-labels label labels) (cdr sofni)))
             labels)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-node-label-construct : DiaFlow-Id->Node-Label
  (lambda [id label style hint]
    (define maybe-label : (U String Void False)
      (let ([labels (default-diaflow-node-label-string)])
        (and labels
             (if (hash? labels)
                 (hash-ref labels id (λ [] #false))
                 (labels id label)))))
    
    (dia-node-text-label id (if (string? maybe-label) maybe-label label) style)))

(define default-diaflow-node-fallback-construct : DiaFlow-Id->Node-Shape
  (lambda [id label style width height direction hint]
    (case/eq (object-name style)
             [(diaflow-start-style) (diaflow-block-terminal id label style width height direction hint)]
             [(diaflow-stop-style) (diaflow-block-terminal id label style width height direction hint)]
             [(diaflow-inspection-style) (diaflow-block-inspection id label style width height direction hint)]
             [(diaflow-reference-style) (diaflow-block-reference id label style width height direction hint)]
             
             [(diaflow-preparation-style) (diaflow-block-preparation id label style width height direction hint)]
             [(diaflow-input-style) (diaflow-block-input id label style width height direction hint)]
             [(diaflow-output-style) (diaflow-block-output id label style width height direction hint)]
             [(diaflow-process-style) (diaflow-block-process id label style width height direction hint)]
             [(diaflow-decision-style) (diaflow-block-decision id label style width height direction hint)]
             [(diaflow-delay-style) (diaflow-block-delay id label style width height direction hint)]
             [(diaflow-operation-style) (diaflow-block-manual-operation id label style width height direction hint)]
             
             [(diaflow-selection-style) (diaflow-block-selection id label style width height direction hint)]
             [(diaflow-junction-style) (diaflow-block-junction id label style width height direction hint)]
             [(diaflow-extract-style) (diaflow-block-extract id label style width height direction hint)]
             [(diaflow-merge-style) (diaflow-block-merge id label style width height direction hint)]
             
             [(diaflow-storage-style) (diaflow-block-storage id label style width height direction hint)]
             [(diaflow-collation-style) (diaflow-block-collation id label style width height direction hint)]
             [(diaflow-sort-style) (diaflow-block-sort id label style width height direction hint)])))

(define default-diaflow-edge-construct : DiaFlow-Arrow->Edge
  (lambda [source target style tracks labels]
    (dia-edge-attach-label
     (dia-edge #:id (dia-edge-id-merge (geo-id source) (and target (geo-id target)) #true)
               #:stroke (dia-edge-select-line-paint style)
               #:source-shape (dia-edge-select-source-shape style)
               #:target-shape (and target (not (dia:node:label? target)) (dia-edge-select-target-shape style))
               tracks)
     labels)))

(define default-diaflow-edge-label-construct : DiaFlow-Arrow->Edge-Label
  (lambda [source target style start end info]
    (make-dia-edge-labels #:font (dia-edge-select-font style)
                          #:font-paint (dia-edge-select-font-paint style)
                          #:rotate? (dia-edge-select-label-rotate? style)
                          #:distance (and (dia-edge-select-label-inline? style) 0.0)
                          start end info)))

(define default-diaflow-free-edge-construct : DiaFlow-Free-Track->Edge
  (lambda [source target style tracks labels]
    (dia-edge-attach-label
     (dia-edge #:id (dia-edge-id-merge source target #false)
               #:stroke (dia-edge-select-line-paint style)
               #:source-shape (dia-edge-select-source-shape style)
               #:target-shape (dia-edge-select-target-shape style)
               tracks)
     labels)))

(define default-diaflow-free-edge-label-construct : DiaFlow-Free-Track->Edge-Label
  (lambda [source target style start end info]
    (make-dia-edge-labels #:font (dia-edge-select-font style)
                          #:font-paint (dia-edge-select-font-paint style)
                          #:rotate? (dia-edge-select-label-rotate? style)
                          #:distance (and (dia-edge-select-label-inline? style) 0.0)
                          start end info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-stick : (-> Geo:Path DiaFlow-Block-Identifier (Option DiaFlow-Id->Node-Shape) DiaFlow-Id->Node-Label
                            DiaFlow-Arrow-Identifier DiaFlow-Arrow->Edge DiaFlow-Arrow->Edge-Label
                            DiaFlow-Free-Track->Edge DiaFlow-Free-Track->Edge-Label Geo-Path-Infobase (Listof Symbol)
                            (Listof (GLayerof Geo)))
  (lambda [master block-identify make-node make-node-label arrow-identify make-arrow make-arrow-label make-free-track make-free-label infobase ignore]
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
                                 [new-node (dia-make-node-layer block-identify make-node make-node-label anchor next-pt direction ignore)])
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
                          (let ([arrows++ (dia-free-track-cons anchor-base ct make-free-track arrows make-free-label infobase)])
                            (stick rest arrows++ nodes++ null #false next-pt))
                          (stick rest arrows nodes null #false next-pt)))]

                   [else (stick rest arrows nodes tracks++ target next-pt)]))

          (append
           
           ;;; NOTE
           ; We need to draw arrows backwards,
           ;   as we usually create archtectural path before jumping back for various branches.
           ;   So that drawing backwards allow main loop path hiding branch paths sharing same routes.
           (reverse arrows)

           (let sort : (Listof (GLayerof Geo)) ([ordered-nodes : (Listof (GLayerof Geo)) null]
                                                [srohcna : (Listof Geo-Anchor-Name) (geo-trail-ranchors gpath)])
             (if (pair? srohcna)
                 (let ([node (hash-ref nodes (car srohcna) (λ [] #false))])
                   (sort (if (not node) ordered-nodes (cons node ordered-nodes)) (cdr srohcna)))
                 ordered-nodes)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-arrow-cons : (-> (GLayerof Dia:Node) (Option (GLayerof Dia:Node)) Geo-Path-Prints
                             DiaFlow-Arrow-Identifier DiaFlow-Arrow->Edge (Listof (GLayerof Geo))
                             DiaFlow-Arrow->Edge-Label Geo-Path-Infobase
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
               
               (define-values (labels sofni) (dia-arrow-label-info-filter infobase ctracks retracks))
               (define edge-style : (Option Dia-Edge-Style) (arrow-identify source target labels))

               (define arrow : (U Dia:Edge Dia:Labeled-Edge Void False)
                 (and edge-style
                      (make-arrow source target edge-style retracks
                                  (dia-label-info->label source target edge-style make-label sofni))))

               (and (geo? arrow) (dia-cons-arrows arrow arrows))))
        
        arrows)))

(define dia-arrow-label-info-filter : (-> Geo-Path-Infobase Geo-Path-Clean-Prints Geo-Path-Clean-Prints
                                          (Values (Listof Dia-Edge-Label-Datum) (Listof DiaFlow-Arrow-Label-Info)))
  (lambda [infobase tracks refined-tracks]
    (let label-filter ([sofni : (Listof DiaFlow-Arrow-Label-Info) null]
                       [slebal : (Listof Dia-Edge-Label-Datum) null]
                       [otracks : Geo-Path-Clean-Prints tracks]
                       [rtracks : Geo-Path-Clean-Prints refined-tracks]
                       [osrc : (Option Float-Complex) #false]
                       [rsrc : Float-Complex 0.0+0.0i])
      (if (and (pair? otracks) (pair? rtracks))

          (let ([oself (car otracks)]
                [rself (car rtracks)])
            (cond [(eq? (gpath:datum-cmd oself) #\M)
                   (label-filter sofni slebal (cdr otracks) (cdr rtracks)
                                 (geo-path-clean-print-position oself)
                                 (geo-path-clean-print-position rself))]
                  [(eq? (gpath:datum-cmd oself) #\L)
                   (let ([otarget (geo-path-clean-print-position oself)]
                         [rtarget (geo-path-clean-print-position rself)])
                     (define maybe-info : (Option Dia-Edge-Label-Datum)
                       (and osrc
                            (let ([info (hash-ref infobase (cons osrc otarget) (λ [] #false))])
                              (dia-edge-label-datum-filter info))))
                     
                     (if (not maybe-info)
                         (label-filter sofni slebal (cdr otracks) (cdr rtracks) otarget rtarget)
                         (label-filter (cons (list rsrc rtarget maybe-info) sofni) (cons maybe-info slebal)
                                       (cdr otracks) (cdr rtracks) otarget rtarget)))]

                  ;;; TODO: deal with curves
                  [else (label-filter sofni slebal (cdr otracks) (cdr rtracks) osrc rsrc)]))
          
          (values (reverse slebal) sofni)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-free-track-cons : (-> (Immutable-HashTable Float-Complex Geo-Anchor-Name)
                                  Geo-Path-Clean-Prints+ DiaFlow-Free-Track->Edge (Listof (GLayerof Geo))
                                  DiaFlow-Free-Track->Edge-Label Geo-Path-Infobase
                                  (Listof (GLayerof Geo)))
  (lambda [anchorbase tracks make-edge arrows make-label infobase]
    (define src-endpt : Float-Complex (geo-path-clean-print-position (car tracks)))
    (define tgt-endpt : Float-Complex (geo-path-clean-print-position (last tracks)))
    (define source : Dia-Free-Edge-Endpoint (hash-ref anchorbase src-endpt (λ [] src-endpt)))
    (define target : Dia-Free-Edge-Endpoint (hash-ref anchorbase tgt-endpt (λ [] tgt-endpt)))
    (define-values (labels sofni) (dia-arrow-label-info-filter infobase tracks tracks))
    (define edge-style : Dia-Edge-Style (dia-edge-style-construct source target labels (default-diaflow-free-track-style-make) make-diaflow-free-track-style))

    (define arrow : (U Dia:Edge Dia:Labeled-Edge Void False)
      (make-edge source target edge-style tracks
                 (dia-label-info->label source target edge-style make-label sofni)))
    
    (if (geo? arrow) (dia-cons-arrows arrow arrows) arrows)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-make-node : (-> DiaFlow-Block-Identifier (Option DiaFlow-Id->Node-Shape) DiaFlow-Id->Node-Label Geo-Anchor-Name (Option Flonum) (Listof Symbol)
                            (Option Dia:Node))
  (lambda [block-identify make-node make-node-label anchor direction ignore]
    (define blk-datum (block-identify anchor))

    (and blk-datum
         (let-values ([(id) (geo-anchor->symbol anchor)]
                      [(style hint) (values (cadr blk-datum) (caddr blk-datum))])
           (define label : (Option Geo) (make-node-label id (car blk-datum) style hint))
           (define-values (width height) (dia-node-smart-size label style))
           (define node : (U Dia:Node Void False)
             (cond [(memq id ignore) #false]
                   [(not make-node) (void)]
                   [else (make-node id label style width height direction hint)]))

           (if (void? node)
               (let ([fallback-node (default-diaflow-node-fallback-construct id label style width height direction hint)])
                 (and (dia:node? fallback-node)
                      fallback-node))
               node)))))

(define dia-make-node-layer : (-> DiaFlow-Block-Identifier (Option DiaFlow-Id->Node-Shape) DiaFlow-Id->Node-Label Geo-Anchor-Name
                                  Float-Complex (Option Flonum) (Listof Symbol)
                                  (Option (GLayerof Dia:Node)))
  (lambda [block-identify make-node make-node-label anchor position direction ignore]
    (define maybe-node (dia-make-node block-identify make-node make-node-label anchor direction ignore))

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

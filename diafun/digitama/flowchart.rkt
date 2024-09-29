#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require geofun/digitama/convert)
(require geofun/digitama/geometry/trail)
(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/footprint)

(require geofun/digitama/layer/sticker)
(require geofun/digitama/layer/type)
(require geofun/digitama/dc/path)

(require "style/flow.rkt")
(require "node/flow.rkt")
(require "node/dc.rkt")
(require "edge/style.rkt")
(require "edge/label.rkt")
(require "edge/refine.rkt")
(require "edge/dc.rkt")
(require "interface/flow.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-node-label-construct : DiaFlow-Anchor->Node-Label
  (lambda [master anchor label style pos]
    (define maybe-label : (U String Void False)
      (let ([labels (default-diaflow-node-label-string)])
        (and labels
             (if (hash? labels)
                 (hash-ref labels anchor (λ [] #false))
                 (labels anchor)))))
    
    (dia-node-text-label anchor (if (string? maybe-label) maybe-label label) style)))

(define default-diaflow-node-construct : DiaFlow-Anchor->Node-Shape
  (lambda [master anchor label style pos hint]
    (define-values (width height) (dia-node-smart-size label style))
    (define node-id : Symbol (geo-anchor->symbol anchor))

    (cond [(diaflow-process-style? style) (diaflow-block-process node-id label style width height)]
          [(diaflow-decision-style? style) (diaflow-block-decision node-id label style width height)]
          [(diaflow-input-style? style) (diaflow-block-dataIO node-id label style width height)]
          [(diaflow-output-style? style) (diaflow-block-dataIO node-id label style width height)]
          [(diaflow-subroutine-style? style) (diaflow-block-subroutine node-id label style width height)]
          [(diaflow-preparation-style? style) (diaflow-block-preparation node-id label style width height)]
          [(diaflow-start-style? style) (diaflow-block-terminal node-id label style width height)]
          [(diaflow-stop-style? style) (diaflow-block-terminal node-id label style width height)]
          [(diaflow-inspection-style? style) (diaflow-block-inspection node-id label style width height hint)]
          [(diaflow-reference-style? style) (diaflow-block-reference node-id label style width height hint)]
          [(diaflow-arrow-label-style? style) (diaflow-block-in-arrow-label node-id label style width height)])))

(define default-diaflow-arrow-construct : DiaFlow-Arrow->Edge
  (lambda [master source target style tracks labels]
    (dia-edge-attach-label
     (dia-edge #:id (dia-edge-id-merge (geo-id (cdr source)) (and target (geo-id (cdr target))) #true)
               #:stroke (dia-edge-select-line-paint style)
               #:source-shape (dia-edge-select-source-shape style)
               #:target-shape (and target (not (dia:node:label? (cdr target))) (dia-edge-select-target-shape style))
               tracks)
     labels)))

(define default-diaflow-arrow-label-construct : DiaFlow-Arrow->Edge-Label
  (lambda [master source target style start end info]
    (make-dia-edge-labels #:font (dia-edge-select-font style)
                          #:font-paint (dia-edge-select-font-paint style)
                          #:rotate? (default-diaflow-edge-label-rotate?)
                          start end info)))

(define default-diaflow-free-track-construct : DiaFlow-Free-Track->Edge
  (lambda [master source target style tracks labels]
    (dia-edge-attach-label
     (dia-edge #:id (dia-edge-id-merge source target #false)
               #:stroke (dia-edge-select-line-paint style)
               #:source-shape (dia-edge-select-source-shape style)
               #:target-shape (dia-edge-select-target-shape style)
               tracks)
     labels)))

(define default-diaflow-free-track-label-construct : DiaFlow-Free-Track->Edge-Label
  (lambda [master source target style start end info]
    (make-dia-edge-labels #:font (dia-edge-select-font style)
                          #:font-paint (dia-edge-select-font-paint style)
                          #:rotate? (default-diaflow-edge-label-rotate?)
                          start end info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-stick : (-> Geo:Path Geo-Anchor->Sticker DiaFlow-Arrow->Edge DiaFlow-Arrow->Edge-Label
                            DiaFlow-Free-Track->Edge DiaFlow-Free-Track->Edge-Label Geo-Path-Infobase
                            (Listof (GLayerof Geo)))
  (lambda [master make-node-sticker make-arrow make-label make-free-track make-free-label infobase]
    (define gpath : Geo-Trail (geo:path-trail master))
    (define anchor-base : (Immutable-HashTable Float-Complex Geo-Anchor-Name) (geo-trail-anchored-positions gpath))
    (define-values (Width Height) (geo-flsize master))

    ; NOTICE: the footprints are initially reversed
    (let stick ([stnirp : (Listof Geo-Path-Print) (geo:path-footprints master)]
                [arrows : (Listof (GLayerof Geo)) null]
                [labels : (Listof (GLayerof Geo)) null]
                [nodes : (HashTable Geo-Anchor-Name (Pairof Geo-Anchor-Name (Option (GLayerof Geo)))) (hasheq)]
                [tracks : (Listof Geo-Path-Print) null]
                [target : (Option (Pairof Geo-Anchor-Name (GLayerof Geo))) #false]
                [last-pt : Float-Complex 0.0+0.0i])
      (if (pair? stnirp)

          (let*-values ([(self rest) (values (car stnirp) (cdr stnirp))])
            (define maybe-pt : (Option Float-Complex) (geo-path-print-position self last-pt))
            (define next-pt : Float-Complex (or maybe-pt last-pt))
            (define anchor : (Option Geo-Anchor-Name) (hash-ref anchor-base maybe-pt (λ [] #false)))
            (define tracks++ : (Pairof Geo-Path-Print (Listof Geo-Path-Print)) (cons self tracks))
            
            (define-values (source nodes++)
              (cond [(not anchor) (values #false nodes)]
                    [(hash-has-key? nodes anchor) (values (hash-ref nodes anchor) nodes)]
                    [else (let ([new-node (cons anchor (geo-sticker-layer master make-node-sticker anchor next-pt 0.0+0.0i Width Height))])
                            (cond [(not new-node) (values #false nodes)]
                                  [else (values new-node (hash-set nodes anchor new-node))]))]))
            
            (cond [(and source (cdr source))
                   (cond [(eq? (car self) #\M)
                          (let-values ([(arrows++ labels++) (dia-arrow-cons master source target tracks++ make-arrow arrows make-label infobase labels)])
                            (stick rest arrows++ labels++ nodes++ null #false next-pt))]
                         [(and target)
                          (let-values ([(arrows++ labels++) (dia-arrow-cons master source target tracks++ make-arrow arrows make-label infobase labels)])
                            (stick rest arrows++ labels++ nodes++ (list self) source next-pt))]
                         [(pair? tracks)
                          (let-values ([(arrows++ labels++) (dia-arrow-cons master source #false tracks++ make-arrow arrows make-label infobase labels)])
                            (stick rest arrows++ labels++ nodes++ (list self) source next-pt))]
                         [else (stick rest arrows labels nodes++ tracks++ source next-pt)])]
                   
                   [(eq? (car self) #\M)
                    (let ([ct (geo-path-cleanse tracks++)])
                      (if (and (pair? ct) (pair? (cdr ct)))
                          (let-values ([(arrows++ labels++) (dia-free-track-cons master anchor-base ct make-free-track arrows make-free-label infobase labels)])
                            (stick rest arrows++ labels++ nodes++ null #false next-pt))
                          (stick rest arrows labels nodes null #false next-pt)))]

                   [else (stick rest arrows labels nodes tracks++ target next-pt)]))
          
          (append arrows labels
                  (for/list : (Listof (GLayerof Geo)) ([n (in-hash-values nodes)] #:when (cdr n))
                    (cdr n)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-arrow-cons : (-> Geo:Path (Pairof Geo-Anchor-Name (GLayerof Geo)) (Option (Pairof Geo-Anchor-Name (GLayerof Geo)))
                             (Listof Geo-Path-Print) DiaFlow-Arrow->Edge (Listof (GLayerof Geo))
                             DiaFlow-Arrow->Edge-Label Geo-Path-Infobase (Listof (GLayerof Geo))
                             (Values (Listof (GLayerof Geo)) (Listof (GLayerof Geo))))
  (lambda [master source target tracks make-arrow arrows make-label infobase alabels]
    (define src-anchor : Geo-Anchor-Name (car source))
    (define tgt-anchor : (Option Geo-Anchor-Name) (and target (car target)))
    (define src-endpt : DiaFlow-Arrow-Endpoint (cons src-anchor (vector-ref (cdr source) 0)))
    (define tgt-endpt : (Option DiaFlow-Arrow-Endpoint) (and target (cons (car target) (vector-ref (cdr target) 0))))
    (define edge-style : Dia-Edge-Style (dia-edge-style-construct src-anchor tgt-anchor (default-diaflow-arrow-style-make) make-diaflow-arrow-style))

    (define maybe-tracks : (Option (Pairof Geo-Path-Clean-Prints Geo-Path-Clean-Prints))
      (let ([ct (geo-path-cleanse tracks)])
        (and (pair? ct) (pair? (cdr ct))
             (cons ct
                   (if (null? (cddr ct))
                       (dia-2-tracks-relocate-endpoints source target ct)
                       (dia-more-tracks-relocate-endpoints source target ct))))))
    
    (define arrow : (U Dia:Edge Dia:Labeled-Edge Void False)
      (and maybe-tracks
           (make-arrow master src-endpt tgt-endpt edge-style (cdr maybe-tracks)
                       (dia-arrow-label-filter master src-endpt tgt-endpt edge-style make-label
                                               infobase (car maybe-tracks) (cdr maybe-tracks)))))
    
    (if (geo? arrow)
        (dia-cons-arrows arrow arrows alabels)
        (values arrows alabels))))

(define dia-arrow-label-filter : (-> Geo:Path DiaFlow-Arrow-Endpoint (Option DiaFlow-Arrow-Endpoint) Dia-Edge-Style
                                     DiaFlow-Arrow->Edge-Label Geo-Path-Infobase Geo-Path-Clean-Prints Geo-Path-Clean-Prints
                                     (Listof Dia-Edge-Label))
  (lambda [master src-endpt tgt-endpt edge-style make-label infobase tracks refined-tracks]
    (let label-filter ([labels : (Listof Dia-Edge-Label) null]
                       [otracks : (Listof Geo-Path-Clean-Print) tracks]
                       [rtracks : (Listof Geo-Path-Clean-Print) refined-tracks]
                       [osrc : (Option Float-Complex) #false]
                       [rsrc : Float-Complex 0.0+0.0i])
      (if (and (pair? otracks) (pair? rtracks))

          (let ([oself (car otracks)]
                [rself (car rtracks)])
            (cond [(eq? (car oself) #\M)
                   (label-filter labels (cdr otracks) (cdr rtracks)
                                 (geo-path-clean-print-position oself)
                                 (geo-path-clean-print-position rself))]
                  [(eq? (car oself) #\L)
                   (let ([otarget (geo-path-clean-print-position oself)]
                         [rtarget (geo-path-clean-print-position rself)])
                     (or (and osrc
                              (let ([info (hash-ref infobase (cons osrc otarget) (λ [] #false))])
                                (and (dia-edge-label-datum? info)
                                     (let ([maybe-labels (make-label master src-endpt tgt-endpt edge-style rsrc rtarget info)])
                                       (cond [(pair? maybe-labels)
                                              (label-filter (append labels maybe-labels)
                                                            (cdr otracks) (cdr rtracks) otarget rtarget)]
                                             [(dia-edge-label? maybe-labels)
                                              (label-filter (append labels (list maybe-labels))
                                                            (cdr otracks) (cdr rtracks) otarget rtarget)]
                                             [else #false])))))
                         (label-filter labels (cdr otracks) (cdr rtracks) otarget rtarget)))]

                  ;;; TODO: deal with curves
                  [else (label-filter labels (cdr otracks) (cdr rtracks) osrc rsrc)]))
          
          labels))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-free-track-cons : (-> Geo:Path (Immutable-HashTable Float-Complex Geo-Anchor-Name)
                                  Geo-Path-Clean-Prints DiaFlow-Free-Track->Edge (Listof (GLayerof Geo))
                                  DiaFlow-Free-Track->Edge-Label Geo-Path-Infobase (Listof (GLayerof Geo))
                                  (Values (Listof (GLayerof Geo)) (Listof (GLayerof Geo))))
  (lambda [master anchorbase tracks make-edge arrows make-label infobase alabels]
    (define src-endpt : Float-Complex (geo-path-clean-print-position (car tracks)))
    (define tgt-endpt : Float-Complex (geo-path-clean-print-position (last tracks)))
    (define source : Dia-Free-Edge-Endpoint (hash-ref anchorbase src-endpt (λ [] src-endpt)))
    (define target : Dia-Free-Edge-Endpoint (hash-ref anchorbase tgt-endpt (λ [] tgt-endpt)))
    (define edge-style : Dia-Edge-Style (dia-edge-style-construct source target (default-diaflow-free-track-style-make) make-diaflow-free-track-style))

    (define arrow : (U Dia:Edge Dia:Labeled-Edge Void False)
      (make-edge master source target edge-style tracks
                 (dia-free-label-filter master source target edge-style make-label
                                        infobase tracks)))
    
    (if (geo? arrow)
        (dia-cons-arrows arrow arrows alabels)
        (values arrows alabels))))

(define dia-free-label-filter : (-> Geo:Path Dia-Free-Edge-Endpoint Dia-Free-Edge-Endpoint Dia-Edge-Style
                                    DiaFlow-Free-Track->Edge-Label Geo-Path-Infobase Geo-Path-Clean-Prints
                                    (Listof Dia-Edge-Label))
  (lambda [master source target edge-style make-label infobase tracks]
    (let label-filter ([labels : (Listof Dia-Edge-Label) null]
                       [tracks : (Listof Geo-Path-Clean-Print) tracks]
                       [src : (Option Float-Complex) #false])
      (if (pair? tracks)

          (let ([oself (car tracks)])
            (cond [(eq? (car oself) #\M) (label-filter labels (cdr tracks) (geo-path-clean-print-position oself))]
                  [(eq? (car oself) #\L)
                   (let ([otarget (geo-path-clean-print-position oself)])
                     (or (and src
                              (let ([info (hash-ref infobase (cons src otarget) (λ [] #false))])
                                (and (dia-edge-label-datum? info)
                                     (let ([maybe-labels (make-label master source target edge-style src otarget info)])
                                       (cond [(pair? maybe-labels) (label-filter (append labels maybe-labels) (cdr tracks) otarget)]
                                             [(dia-edge-label? maybe-labels) (label-filter (append labels (list maybe-labels)) (cdr tracks) otarget)]
                                             [else #false])))))
                         (label-filter labels (cdr tracks) otarget)))]

                  ;;; TODO: deal with curves
                  [else (label-filter labels (cdr tracks) src)]))
          
          labels))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-cons-arrows : (-> (U Dia:Edge Dia:Labeled-Edge) (Listof (GLayerof Geo)) (Listof (GLayerof Geo))
                              (Values (Listof (GLayerof Geo)) (Listof (GLayerof Geo))))
  (lambda [arrow arrows alabels]
    (define ppos : Float-Complex (dia-edge-self-pin-position arrow #false))
    (define-values (awidth aheight) (geo-flsize arrow))
    (define alayer (vector-immutable arrow (real-part ppos) (imag-part ppos) awidth aheight))

    (values (cons alayer arrows) alabels)))

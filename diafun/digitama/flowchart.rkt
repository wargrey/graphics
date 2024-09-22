#lang typed/racket/base

(provide (all-defined-out))

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
(require "edge/refine.rkt")
(require "edge/dc.rkt")
(require "interface/flow.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-node-construct : DiaFlow-Anchor->Node
  (lambda [master anchor pos node-id style]
    (define maybe-label : (U String Void False) ((default-diaflow-node-label-string) node-id))
    (define label-text : String (if (string? maybe-label) maybe-label (geo-anchor->string node-id)))
    (define-values (label width height) (dia-node-extent node-id label-text style))
    
    (cond [(diaflow-process-style? style) (diaflow-block-process node-id label style width height)]
          [(diaflow-decision-style? style) (diaflow-block-decision node-id label style width height)]
          [(diaflow-input-style? style) (diaflow-block-dataIO node-id label style width height)]
          [(diaflow-output-style? style) (diaflow-block-dataIO node-id label style width height)]
          [(diaflow-subroutine-style? style) (diaflow-block-subroutine node-id label style width height)]
          [(diaflow-preparation-style? style) (diaflow-block-preparation node-id label style width height)]
          [(diaflow-start-style? style) (diaflow-block-terminal node-id label style width height)]
          [(diaflow-stop-style? style) (diaflow-block-terminal node-id label style width height)]
          [(diaflow-arrow-label-style? style) (diaflow-block-in-arrow-label node-id label style width height)])))

(define default-diaflow-arrow-construct : DiaFlow-Arrow->Edge
  (lambda [master source target style tracks]
    (dia-edge #:id (dia-edge-id-merge (geo-id (cdr source)) (and target (geo-id (cdr target))) #true)
              #:stroke (dia-edge-select-line-paint style)
              #:source-shape (dia-edge-select-source-shape style)
              #:target-shape (and target (not (dia:node:label? (cdr target))) (dia-edge-select-target-shape style))
              tracks)))

(define default-diaflow-arrow-label-construct : DiaFlow-Arrow-Label-Sticker
  (lambda [master source target style start-pos end-pos label]
    (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-stick : (->* (Geo:Path Geo-Anchor->Sticker DiaFlow-Arrow->Edge DiaFlow-Arrow-Label-Sticker Geo-Path-Infobase)
                             ((Option Geo-Trusted-Anchors))
                             (Listof (GLayerof Geo)))
  (lambda [master make-node-sticker make-arrow make-label infobase [trusted-anchors #false]]
    (define gpath : Geo-Trail (geo:path-trail master))
    (define anchor-base : (Immutable-HashTable Float-Complex Geo-Anchor-Name) (geo-trail-anchored-positions gpath trusted-anchors))
    (define-values (Width Height) (geo-flsize master))

    ; WARNING: the footprints are initially reversed
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
            
            (if (and source (cdr source))
                (cond [(eq? (car self) #\M)
                       (let-values ([(arrows++ labels++) (dia-arrow-cons master source target tracks++ make-arrow arrows make-label infobase labels)])
                         (stick rest arrows++ labels++ nodes++ null #false next-pt))]
                      [(and target)
                       (let-values ([(arrows++ labels++) (dia-arrow-cons master source target tracks++ make-arrow arrows make-label infobase labels)])
                         (stick rest arrows++ labels++ nodes++ (list self) source next-pt))]
                      [(pair? tracks)
                       (let-values ([(arrows++ labels++) (dia-arrow-cons master source #false tracks++ make-arrow arrows make-label infobase labels)])
                         (stick rest arrows++ labels++ nodes++ (list self) source next-pt))]
                      [else (stick rest arrows labels nodes++ tracks++ source next-pt)])
                (stick rest arrows labels nodes tracks++ target next-pt)))
          
          (append arrows labels
                  (for/list : (Listof (GLayerof Geo)) ([n (in-hash-values nodes)] #:when (cdr n))
                    (cdr n)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-arrow-cons : (-> Geo:Path (Pairof Geo-Anchor-Name (GLayerof Geo)) (Option (Pairof Geo-Anchor-Name (GLayerof Geo)))
                             (Listof Geo-Path-Print) DiaFlow-Arrow->Edge (Listof (GLayerof Geo))
                             DiaFlow-Arrow-Label-Sticker Geo-Path-Infobase (Listof (GLayerof Geo))
                             (Values (Listof (GLayerof Geo)) (Listof (GLayerof Geo))))
  (lambda [master source target tracks make-arrow arrows make-label infobase alabels]
    (define src-anchor : Geo-Anchor-Name (car source))
    (define tgt-anchor : (Option Geo-Anchor-Name) (and target (car target)))
    (define src-endpt : DiaFlow-Arrow-Endpoint (cons src-anchor (vector-ref (cdr source) 0)))
    (define tgt-endpt : (Option DiaFlow-Arrow-Endpoint) (and target (cons (car target) (vector-ref (cdr target) 0))))
    
    (define arrow : (U Dia:Edge Void False)
      (let ([ct (geo-path-cleanse tracks)])
        (and (pair? ct) (pair? (cdr ct))
             (make-arrow master src-endpt tgt-endpt
                         (dia-edge-style-construct src-anchor tgt-anchor (default-diaflow-arrow-style-make) make-diaflow-arrow-style)
                         (if (null? (cddr ct))
                             (dia-2-tracks-relocate-endpoints source target ct)
                             (dia-more-tracks-relocate-endpoints source target ct))))))
    
    (if (dia:edge? arrow)
        
        (let ([ppos (dia-edge-pin-at-position arrow #false)])
          (define-values (awidth aheight) (geo-flsize arrow))
          (define alayer (vector-immutable arrow (real-part ppos) (imag-part ppos) awidth aheight))
          (values (cons alayer arrows) alabels))

        (values arrows alabels))))

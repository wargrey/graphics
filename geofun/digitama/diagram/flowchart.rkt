#lang typed/racket/base

(provide (all-defined-out))

(require "style/node.rkt")
(require "style/flow.rkt")
(require "shape/flow.rkt")

(require "../convert.rkt")
(require "../unsafe/path.rkt")
(require "../geometry/bbox.rkt")
(require "../geometry/trail.rkt")
(require "../geometry/anchor.rkt")
(require "../geometry/footprint.rkt")
(require "../diagram/style/edge.rkt")

(require "../layer/sticker.rkt")
(require "../layer/type.rkt")

(require "../dc/edge.rkt")
(require "../dc/path.rkt")
(require "../dc/paint.rkt")
(require "../../paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Flow-Arrow->Edge (-> Geo:Path Geo-Anchor-Name (Option Geo-Anchor-Name) (Listof Geo-Path-Print) (Option Geo:Edge)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-flow-chart-node-constructor : Geo-Anchor->Sticker
  (lambda [self anchor pos Width Height]
    (define-values (text style) (geo-flow-block-detect anchor))

    (and style
         (parameterize ([default-geo-node-base-style make-geo-flow-node-style])
           (define-values (label width height) (geo-node-extent text style))
           (cond [(geo-flow-process-style? style) (geo-flow-block-process label style width height)]
                 [(geo-flow-decision-style? style) (geo-flow-block-decision label style width height)]
                 [(geo-flow-input-style? style) (geo-flow-block-dataIO label style width height)]
                 [(geo-flow-output-style? style) (geo-flow-block-dataIO label style width height)]
                 [(geo-flow-preparation-style? style) (geo-flow-block-preparation label style width height)]
                 [(geo-flow-start-style? style) (geo-flow-block-terminal label style width height)]
                 [(geo-flow-stop-style? style) (geo-flow-block-terminal label style width height)])))))

(define default-flow-chart-arrow-constructor : Geo-Flow-Arrow->Edge
  (lambda [master source target footprints]
    (define style : Geo-Edge-Style
      (geo-edge-style-construct source target
                                (default-flow-arrow-style-make)
                                make-geo-flow-arrow-style))

    (geo-edge footprints #:stroke (geo-edge-select-line-paint style))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-flow-stick : (->* (Geo:Path Geo-Anchor->Sticker Geo-Flow-Arrow->Edge) ((Option Geo-Trusted-Anchors)) (Listof (GLayerof Geo)))
  (lambda [master make-node make-arrow [trusted-anchors #false]]
    (define gpath : Geo-Trail (geo:path-trail master))
    (define apositions : (Immutable-HashTable Float-Complex Geo-Anchor-Name) (geo-trail-anchored-positions gpath trusted-anchors))
    (define-values (Width Height) (geo-flsize master))

    ; NOTE: the footprints are initially reversed
    (let stick ([stnirp : (Listof Geo-Path-Print) (geo:path-footprints master)]
                [arrows : (Listof (GLayerof Geo)) null]
                [nodes : (Listof (GLayerof Geo)) null]
                [tracks : (Listof Geo-Path-Print) null]
                [target : (Option (Pairof Geo-Anchor-Name (GLayerof Geo))) #false]
                [last-pt : Float-Complex 0.0+0.0i])
      (if (pair? stnirp)
          (let*-values ([(self rest) (values (car stnirp) (cdr stnirp))])
            (define maybe-pt (geo-path-print-position self last-pt))
            (define next-pt (or maybe-pt last-pt))
            (define tracks++ (cons self tracks))
            (define anchor (hash-ref apositions maybe-pt (λ [] #false)))
            (define node (and anchor (geo-sticker-layer master make-node anchor (or maybe-pt last-pt) 0.0+0.0i 0.0+0.0i Width Height)))
            (define source (and anchor node (cons anchor node)))
            (if (and source)
                (let ([nodes++ (cons (cdr source) nodes)])
                  (cond [(and target)
                         (if (eq? (car self) #\M)
                             (stick rest (geo-arrow-cons master source target tracks++ make-arrow arrows) nodes++ null #false next-pt)
                             (stick rest (geo-arrow-cons master source target tracks++ make-arrow arrows) nodes++ (list self) source next-pt))]
                        [(pair? tracks)
                         (stick rest (geo-arrow-cons master source #false tracks++ make-arrow arrows) nodes++ (list self) source next-pt)]
                        [else (stick rest arrows nodes++ tracks++ source next-pt)]))
                (stick rest arrows nodes tracks++ source next-pt)))
          (append arrows nodes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-arrow-cons : (-> Geo:Path (Pairof Geo-Anchor-Name (GLayerof Geo)) (Option (Pairof Geo-Anchor-Name (GLayerof Geo)))
                             (Listof Geo-Path-Print) Geo-Flow-Arrow->Edge
                             (Listof (GLayerof Geo)) (Listof (GLayerof Geo)))
  (lambda [master source target footprints make-arrow arrows]
    (define arrow : (Option Geo:Edge) (make-arrow master (car source) (and target (car target)) footprints))

    (if (geo:edge? arrow)
        (let ([ppos (geo-edge-pin-at-position arrow #false)])
          (define-values (awidth aheight) (geo-flsize arrow))
          (define alayer (vector-immutable arrow (real-part ppos) (imag-part ppos) awidth aheight))
          (cons alayer arrows))
        arrows)))

(define geo-flow-chart-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:path?])
      (define-values (xoff yoff) (geo-bbox-offset-values (geo:path-bbox self)))
      (path_stamp (reverse (geo:path-footprints self)) xoff yoff
                  (current-stroke-source) (current-fill-source) (default-fill-rule)
                  (default-geometry-density)))))

#lang typed/racket/base

(provide (all-defined-out))

(require "style/flow.rkt")
(require "node/flow.rkt")
(require "edge/style.rkt")
(require "edge/dc.rkt")

(require geofun/digitama/convert)
(require geofun/digitama/geometry/trail)
(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/footprint)

(require geofun/digitama/layer/sticker)
(require geofun/digitama/layer/type)
(require geofun/digitama/dc/path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DiaFlow-Arrow-Endpoint (Pairof Geo-Anchor-Name Geo))
(define-type DiaFlow-Arrow->Edge (-> Geo:Path DiaFlow-Arrow-Endpoint (Option DiaFlow-Arrow-Endpoint)
                                     (List* Geo-Path-Clean-Print Geo-Path-Clean-Print (Listof Geo-Path-Clean-Print))
                                     (Option Dia:Edge)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-node-constructor : Geo-Anchor->Sticker
  (lambda [self anchor pos Width Height]
    (define-values (node-key style) (diaflow-block-detect anchor))

    (and style
         (parameterize ([default-dia-node-base-style make-diaflow-node-style])
           (define-values (label width height) (dia-node-extent node-key ((default-diaflow-label-construct) node-key) style))
           (cond [(diaflow-process-style? style) (diaflow-block-process node-key label style width height)]
                 [(diaflow-decision-style? style) (diaflow-block-decision node-key label style width height)]
                 [(diaflow-input-style? style) (diaflow-block-dataIO node-key label style width height)]
                 [(diaflow-output-style? style) (diaflow-block-dataIO node-key label style width height)]
                 [(diaflow-preparation-style? style) (diaflow-block-preparation node-key label style width height)]
                 [(diaflow-start-style? style) (diaflow-block-terminal node-key label style width height)]
                 [(diaflow-stop-style? style) (diaflow-block-terminal node-key label style width height)])))))

(define default-diaflow-arrow-constructor : DiaFlow-Arrow->Edge
  (lambda [master source target tracks]
    (define src-id : String (geo-anchor->string (geo-id (cdr source))))
    (define tgt-id : (Option String) (and target (geo-anchor->string (geo-id (cdr target)))))
    (define edge-id : String (if (not tgt-id) (string-append src-id "-.") (string-append src-id "->" tgt-id)))
    
    (define style : Dia-Edge-Style
      (dia-edge-style-construct (car source) (and target (car target))
                                (default-diaflow-arrow-style-make)
                                make-diaflow-arrow-style))

    (dia-edge #:id (string->symbol edge-id)
              #:stroke (dia-edge-select-line-paint style)
              #:source-shape (dia-edge-select-source-shape style)
              #:target-shape (and target (dia-edge-select-target-shape style))
              tracks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-stick : (->* (Geo:Path Geo-Anchor->Sticker DiaFlow-Arrow->Edge) ((Option Geo-Trusted-Anchors)) (Listof (GLayerof Geo)))
  (lambda [master make-node make-arrow [trusted-anchors #false]]
    (define gpath : Geo-Trail (geo:path-trail master))
    (define apositions : (Immutable-HashTable Float-Complex Geo-Anchor-Name) (geo-trail-anchored-positions gpath trusted-anchors))
    (define-values (Width Height) (geo-flsize master))

    ; WARNING: the footprints are initially reversed
    (let stick ([stnirp : (Listof Geo-Path-Print) (geo:path-footprints master)]
                [arrows : (Listof (GLayerof Geo)) null]
                [nodes : (Listof (GLayerof Geo)) null]
                [tracks : (Listof Geo-Path-Print) null]
                [target : (Option (Pairof Geo-Anchor-Name (GLayerof Geo))) #false]
                [last-pt : Float-Complex 0.0+0.0i])
      (if (pair? stnirp)
          (let*-values ([(self rest) (values (car stnirp) (cdr stnirp))])
            (define maybe-pt : (Option Float-Complex) (geo-path-print-position self last-pt))
            (define next-pt : Float-Complex (or maybe-pt last-pt))
            (define anchor : (Option Geo-Anchor-Name) (hash-ref apositions maybe-pt (λ [] #false)))
            (define node : (Option (GLayerof Geo)) (and anchor (geo-sticker-layer master make-node anchor next-pt 0.0+0.0i 0.0+0.0i Width Height)))
            (define source : (Option (Pairof Geo-Anchor-Name (GLayerof Geo))) (and anchor node (cons anchor node)))
            (define tracks++ : (Pairof Geo-Path-Print (Listof Geo-Path-Print)) (cons self tracks))

            (if (and source)
                (let ([nodes++ (cons (cdr source) nodes)])
                  (cond [(and target)
                         (let ([arrows++ (dia-arrow-cons master source target tracks++ make-arrow arrows)])
                           (if (eq? (car self) #\M)
                               (stick rest arrows++ nodes++ null #false next-pt)
                               (stick rest arrows++ nodes++ (list self) source next-pt)))]
                        [(pair? tracks)
                         (let ([arrows++ (dia-arrow-cons master source #false tracks++ make-arrow arrows)])
                           (stick rest arrows++ nodes++ (list self) source next-pt))]
                        [else (stick rest arrows nodes++ tracks++ source next-pt)]))
                (stick rest arrows nodes tracks++ target next-pt)))
          (append arrows nodes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-arrow-cons : (-> Geo:Path (Pairof Geo-Anchor-Name (GLayerof Geo)) (Option (Pairof Geo-Anchor-Name (GLayerof Geo)))
                             (Listof Geo-Path-Print) DiaFlow-Arrow->Edge
                             (Listof (GLayerof Geo)) (Listof (GLayerof Geo)))
  (lambda [master source target tracks make-arrow arrows]
    (define clean-tracks : (Listof Geo-Path-Clean-Print) (geo-path-cleanse tracks))

    (define arrow : (Option Dia:Edge)
      (and (pair? clean-tracks)
           (pair? (cdr clean-tracks))
           (make-arrow master
                       (cons (car source) (vector-ref (cdr source) 0))
                       (and target (cons (car target) (vector-ref (cdr target) 0)))
                       (if (null? (cddr clean-tracks))
                           (dia-2-tracks-relocate-endpoints source target clean-tracks)
                           (dia-more-tracks-relocate-endpoints source target clean-tracks)))))

    (if (and arrow)
        (let ([ppos (dia-edge-pin-at-position arrow #false)])
          (define-values (awidth aheight) (geo-flsize arrow))
          (define alayer (vector-immutable arrow (real-part ppos) (imag-part ppos) awidth aheight))
          (cons alayer arrows))
        arrows)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO: deal with curved prints
(define dia-2-tracks-relocate-endpoints : (-> (Option (Pairof Geo-Anchor-Name (GLayerof Geo))) (Option (Pairof Geo-Anchor-Name (GLayerof Geo)))
                                              (List Geo-Path-Clean-Print Geo-Path-Clean-Print)
                                              (List Geo-Path-Clean-Print Geo-Path-Clean-Print))
  (lambda [source target tracks]
    (define A (geo-path-clean-print-position (car tracks)))
    (define B (geo-path-clean-print-position (cadr tracks)))

    (if (and A B)
        (let*-values ([(v) (- B A)]
                      [(aw/2 ah/2) (if (not source) (values 0.0 0.0) (geo-layer-size (cdr source) 0.5))]
                      [(bw/2 bh/2) (if (not target) (values 0.0 0.0) (geo-layer-size (cdr target) 0.5))]
                      [(dAx dBx) (if (= (real-part v) 0.0) (values 0.0 0.0) (values aw/2 bw/2))]
                      [(dAy dBy) (if (= (imag-part v) 0.0) (values 0.0 0.0) (values ah/2 bh/2))])
          (list (if (not source) (car tracks)  (cons (caar tracks)  (+ A (make-rectangular dAx dAy))))
                (if (not target) (cadr tracks) (cons (caadr tracks) (- B (make-rectangular dBx dBy))))))
        tracks)))

(define dia-more-tracks-relocate-endpoints : (-> (Pairof Geo-Anchor-Name (GLayerof Geo)) (Option (Pairof Geo-Anchor-Name (GLayerof Geo)))
                                                 (List* Geo-Path-Clean-Print Geo-Path-Clean-Print Geo-Path-Clean-Print (Listof Geo-Path-Clean-Print))
                                                 (List* Geo-Path-Clean-Print Geo-Path-Clean-Print (Listof Geo-Path-Clean-Print)))
  (lambda [source target tracks]
    (define h1st : Geo-Path-Clean-Print (car tracks))
    (define h2nd : Geo-Path-Clean-Print (cadr tracks))
    (define re:head (dia-2-tracks-relocate-endpoints source #false (list h1st h2nd)))
    
    (let relocate ([t2nd : Geo-Path-Clean-Print h2nd]
                   [t1st : Geo-Path-Clean-Print (caddr tracks)]
                   [skcart : (Listof Geo-Path-Clean-Print) (list h2nd)]
                   [tracks : (Listof Geo-Path-Clean-Print) (cdddr tracks)])
      (if (null? tracks)
          (let ([re:tail (dia-2-tracks-relocate-endpoints #false target (list t2nd t1st))]
                [body (assert (reverse skcart) pair?)])
            (list* (car re:head) (car body) (append (cdr body) (cdr re:tail))))
          (relocate t1st (car tracks) (cons t1st skcart) (cdr tracks))))))

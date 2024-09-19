#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/symbol)

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
(require "edge/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DiaFlow-Anchor->Node (-> Geo:Path Geo-Anchor-Name Float-Complex Symbol Dia-Node-Style (U Geo-Sticker-Datum Void False)))

(define-type DiaFlow-Arrow-Endpoint (Pairof Geo-Anchor-Name Geo))
(define-type DiaFlow-Arrow->Edge (-> Geo:Path DiaFlow-Arrow-Endpoint (Option DiaFlow-Arrow-Endpoint) Dia-Edge-Style
                                     (List* Geo-Path-Clean-Print Geo-Path-Clean-Print (Listof Geo-Path-Clean-Print))
                                     (U Dia:Edge Void False)))

(define-type DiaFlow-Arrow-Label-Stickers (-> Geo:Path DiaFlow-Arrow-Endpoint (Option DiaFlow-Arrow-Endpoint) Dia-Edge-Style
                                              (Pairof Float-Complex Flonum) (Pairof Float-Complex Flonum)
                                              (Values (U Geo-Sticker-Datum Void False) (U Geo-Sticker-Datum Void False))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-node-constructor : DiaFlow-Anchor->Node
  (lambda [master anchor pos node-id style]
    (define maybe-label : (U String Void False) ((default-diaflow-node-label-string) node-id))
    (define label-text : String (if (string? maybe-label) maybe-label (symbol->immutable-string node-id)))
    (define-values (label width height) (dia-node-extent node-id (if (string? maybe-label) maybe-label (symbol->immutable-string node-id)) style))
    
    (cond [(diaflow-process-style? style) (diaflow-block-process node-id label style width height)]
          [(diaflow-decision-style? style) (diaflow-block-decision node-id label style width height)]
          [(diaflow-input-style? style) (diaflow-block-dataIO node-id label style width height)]
          [(diaflow-output-style? style) (diaflow-block-dataIO node-id label style width height)]
          [(diaflow-preparation-style? style) (diaflow-block-preparation node-id label style width height)]
          [(diaflow-start-style? style) (diaflow-block-terminal node-id label style width height)]
          [(diaflow-stop-style? style) (diaflow-block-terminal node-id label style width height)])))

(define default-diaflow-arrow-constructor : DiaFlow-Arrow->Edge
  (lambda [master source target style tracks]
    (dia-edge #:id (dia-edge-id-merge (geo-id (cdr source)) (and target (geo-id (cdr target))) #true)
              #:stroke (dia-edge-select-line-paint style)
              #:source-shape (dia-edge-select-source-shape style)
              #:target-shape (and target (dia-edge-select-target-shape style))
              tracks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-stick : (->* (Geo:Path DiaFlow-Anchor->Node DiaFlow-Arrow->Edge (Option DiaFlow-Arrow-Label-Stickers))
                             ((Option Geo-Trusted-Anchors))
                             (Listof (GLayerof Geo)))
  (lambda [master make-node make-arrow make-label [trusted-anchors #false]]
    (define make-node-stick : Geo-Anchor->Sticker (diaflow-node-sticker make-node))
    (define gpath : Geo-Trail (geo:path-trail master))
    (define apositions : (Immutable-HashTable Float-Complex Geo-Anchor-Name) (geo-trail-anchored-positions gpath trusted-anchors))
    (define-values (Width Height) (geo-flsize master))

    ; WARNING: the footprints are initially reversed
    (let stick ([stnirp : (Listof Geo-Path-Print) (geo:path-footprints master)]
                [arrows : (Listof (GLayerof Geo)) null]
                [alabels : (Listof (GLayerof Geo)) null]
                [nodes : (HashTable Geo-Anchor-Name (Pairof Geo-Anchor-Name (Option (GLayerof Geo)))) (hasheq)]
                [tracks : (Listof Geo-Path-Print) null]
                [target : (Option (Pairof Geo-Anchor-Name (GLayerof Geo))) #false]
                [last-pt : Float-Complex 0.0+0.0i])
      (if (pair? stnirp)

          (let*-values ([(self rest) (values (car stnirp) (cdr stnirp))])
            (define maybe-pt : (Option Float-Complex) (geo-path-print-position self last-pt))
            (define next-pt : Float-Complex (or maybe-pt last-pt))
            (define anchor : (Option Geo-Anchor-Name) (hash-ref apositions maybe-pt (λ [] #false)))
            (define tracks++ : (Pairof Geo-Path-Print (Listof Geo-Path-Print)) (cons self tracks))
            
            (define-values (source nodes++)
              (cond [(not anchor) (values #false nodes)]
                    [(hash-has-key? nodes anchor) (values (hash-ref nodes anchor) nodes)]
                    [else (let ([new-node (cons anchor (geo-sticker-layer master make-node-stick anchor next-pt 0.0+0.0i Width Height))])
                            (cond [(not new-node) (values #false nodes)]
                                  [else (values new-node (hash-set nodes anchor new-node))]))]))
            
            (if (and source (cdr source))
                (cond [(eq? (car self) #\M)
                       (let-values ([(arrows++ labels++) (dia-arrow-cons master source target tracks++ make-arrow arrows alabels)])
                         (stick rest arrows++ labels++ nodes++ null #false next-pt))]
                      [(and target)
                       (let-values ([(arrows++ labels++) (dia-arrow-cons master source target tracks++ make-arrow arrows alabels)])
                         (stick rest arrows++ labels++ nodes++ (list self) source next-pt))]
                      [(pair? tracks)
                       (let-values ([(arrows++ labels++) (dia-arrow-cons master source #false tracks++ make-arrow arrows alabels)])
                         (stick rest arrows++ labels++ nodes++ (list self) source next-pt))]
                      [else (stick rest arrows alabels nodes++ tracks++ source next-pt)])
                (stick rest arrows alabels nodes tracks++ target next-pt)))
          
          (append arrows alabels
                  (for/list : (Listof (GLayerof Geo)) ([n (in-hash-values nodes)] #:when (cdr n))
                    (cdr n)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-arrow-cons : (-> Geo:Path (Pairof Geo-Anchor-Name (GLayerof Geo)) (Option (Pairof Geo-Anchor-Name (GLayerof Geo)))
                             (Listof Geo-Path-Print) DiaFlow-Arrow->Edge (Listof (GLayerof Geo)) (Listof (GLayerof Geo))
                             (Values (Listof (GLayerof Geo)) (Listof (GLayerof Geo))))
  (lambda [master source target tracks make-arrow arrows alabels]
    (define clean-tracks : (Listof Geo-Path-Clean-Print) (geo-path-cleanse tracks))
    (define src-anchor : Geo-Anchor-Name (car source))
    (define tgt-anchor : (Option Geo-Anchor-Name) (and target (car target)))

    (define arrow : (U Dia:Edge Void False)
      (and (pair? clean-tracks)
           (pair? (cdr clean-tracks))
           (make-arrow master
                       (cons src-anchor (vector-ref (cdr source) 0)) (and target (cons (car target) (vector-ref (cdr target) 0)))
                       (dia-edge-style-construct src-anchor tgt-anchor (default-diaflow-arrow-style-make) make-diaflow-arrow-style)
                       (if (null? (cddr clean-tracks))
                           (dia-2-tracks-relocate-endpoints source target clean-tracks)
                           (dia-more-tracks-relocate-endpoints source target clean-tracks)))))
    
    (if (dia:edge? arrow)
        (let ([ppos (dia-edge-pin-at-position arrow #false)])
          (define-values (awidth aheight) (geo-flsize arrow))
          (define alayer (vector-immutable arrow (real-part ppos) (imag-part ppos) awidth aheight))
          (values (cons alayer arrows) alabels))
    (values arrows alabels))))

(define diaflow-node-sticker : (-> DiaFlow-Anchor->Node Geo-Anchor->Sticker)
  (lambda [make-node]
    (λ [master anchor pos Width Height]
      (define-values (node-key style) (diaflow-block-detect anchor))
      (and style (make-node master anchor pos node-key style)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-block-detect : (-> Geo-Anchor-Name (Values Symbol (Option Dia-Node-Style)))
  (lambda [anchor]
    (if (keyword? anchor)
        (let ([text (geo-anchor->string anchor)])
          (cond [(or (string-ci=? text "home") (string-ci=? text "start"))
                 (diaflow-node-style-construct (default-diaflow-canonical-start-name) anchor (default-diaflow-start-style-make) make-diaflow-start-style)]
                [(or (string-ci=? text "end") (string-ci=? text "terminate"))
                 (diaflow-node-style-construct (default-diaflow-canonical-stop-name) anchor (default-diaflow-stop-style-make) make-diaflow-stop-style)]
                [else (values 'who-cares #false)]))
        (let ([text (geo-anchor->string anchor)])
          (define size (string-length text))
          (cond [(string-suffix? text "?")
                 (diaflow-node-style-construct text anchor (default-diaflow-decision-style-make) make-diaflow-decision-style)]
                [(string-suffix? text "!")
                 (diaflow-node-style-construct text anchor (default-diaflow-preparation-style-make) make-diaflow-preparation-style)]
                [(string-prefix? text "^")
                 (diaflow-node-style-construct (substring text 1 size) anchor (default-diaflow-start-style-make) make-diaflow-start-style)]
                [(string-suffix? text "$")
                 (diaflow-node-style-construct (substring text 0 (sub1 size)) anchor (default-diaflow-stop-style-make) make-diaflow-stop-style)]
                [(string-prefix? text ">>")
                 (diaflow-node-style-construct (substring text 2 size) anchor (default-diaflow-input-style-make) make-diaflow-input-style)]
                [(string-suffix? text "<<")
                 (diaflow-node-style-construct (substring text 0 (- size 2)) anchor (default-diaflow-output-style-make) make-diaflow-output-style)]
                [(string-prefix? text "//")
                 (diaflow-node-style-construct (substring text 2 size) anchor (default-diaflow-comment-style-make) make-diaflow-comment-style)]
                [(string-prefix? text "->")
                 (diaflow-node-style-construct (substring text 2 size) anchor (default-diaflow-subroutine-style-make) make-diaflow-subroutine-style)]
                [(string-prefix? text "@")
                 (diaflow-node-style-construct (substring text 1 size) anchor (default-diaflow-inspection-style-make) make-diaflow-inspection-style)]
                [(string-prefix? text "&")
                 (diaflow-node-style-construct (substring text 1 size) anchor (default-diaflow-reference-style-make) make-diaflow-reference-style)]
                [(> size 0) (diaflow-node-style-construct text anchor (default-diaflow-process-style-make) make-diaflow-process-style)]
                [else (values 'who-cares #false)])))))

(define #:forall (S) diaflow-node-style-construct : (-> String Geo-Anchor-Name (Option (Dia-Node-Style-Make* S)) (-> S) (Values Symbol S))
  (lambda [text anchor mk-style mk-fallback-style]
    (dia-node-style-construct text anchor mk-style mk-fallback-style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO: deal with curved prints
(define dia-2-tracks-relocate-source : (-> (Pairof Geo-Anchor-Name (GLayerof Geo)) Geo-Path-Clean-Print Geo-Path-Clean-Print Geo-Path-Clean-Print)
  (lambda [source a:track b:track]
    (define A : Float-Complex (geo-path-clean-print-position a:track))
    (define B : Float-Complex (geo-path-clean-print-position b:track))
    
    (cons (car b:track)
          (or (dia-line-node-intersect (cdr source) A B A)
              A))))

(define dia-2-tracks-relocate-target : (-> (Pairof Geo-Anchor-Name (GLayerof Geo)) Geo-Path-Clean-Print Geo-Path-Clean-Print Geo-Path-Clean-Print)
  (lambda [target a:track b:track]
    (define A : Float-Complex (geo-path-clean-print-position a:track))
    (define B : Float-Complex (geo-path-clean-print-position b:track))

    (cons (car b:track)
          (or (dia-line-node-intersect (cdr target) A B B)
              B))))

(define dia-2-tracks-relocate-endpoints : (-> (Option (Pairof Geo-Anchor-Name (GLayerof Geo))) (Option (Pairof Geo-Anchor-Name (GLayerof Geo)))
                                              (List Geo-Path-Clean-Print Geo-Path-Clean-Print)
                                              (List Geo-Path-Clean-Print Geo-Path-Clean-Print))
  (lambda [source target tracks]
    (define a:track : Geo-Path-Clean-Print (car tracks))
    (define b:track : Geo-Path-Clean-Print (cadr tracks))
    
    (list (if (not source) a:track (dia-2-tracks-relocate-source source a:track b:track))
          (if (not target) b:track (dia-2-tracks-relocate-target target a:track b:track)))))

(define dia-more-tracks-relocate-endpoints : (-> (Pairof Geo-Anchor-Name (GLayerof Geo)) (Option (Pairof Geo-Anchor-Name (GLayerof Geo)))
                                                 (List* Geo-Path-Clean-Print Geo-Path-Clean-Print Geo-Path-Clean-Print (Listof Geo-Path-Clean-Print))
                                                 (List* Geo-Path-Clean-Print Geo-Path-Clean-Print (Listof Geo-Path-Clean-Print)))
  (lambda [source target tracks]
    (define h1st : Geo-Path-Clean-Print (car tracks))
    (define h2nd : Geo-Path-Clean-Print (cadr tracks))
    (define re:head : Geo-Path-Clean-Print (dia-2-tracks-relocate-source source h1st h2nd))
    
    (let relocate ([t2nd : Geo-Path-Clean-Print h2nd]
                   [t1st : Geo-Path-Clean-Print (caddr tracks)]
                   [skcart : (Listof Geo-Path-Clean-Print) (list h2nd)]
                   [tracks : (Listof Geo-Path-Clean-Print) (cdddr tracks)])
      (if (null? tracks)
          (let-values ([(re:tail) (if (not target) t1st (dia-2-tracks-relocate-target target t2nd t1st))]
                       [(body) (assert (reverse skcart) pair?)])
            (list* re:head (car body) (append (cdr body) (list re:tail))))
          (relocate t1st (car tracks) (cons t1st skcart) (cdr tracks))))))

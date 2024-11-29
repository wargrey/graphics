#lang typed/racket/base

(provide (all-defined-out))

(require racket/vector)
(require digimon/sequence)
(require digimon/function)

(require "../paint.rkt")
(require "../../paint.rkt")

(require "../convert.rkt")
(require "../composite.rkt")

(require "../geometry/ink.rkt")
(require "../layer/type.rkt")
(require "../layer/combine.rkt")
(require "../layer/table.rkt")

(require "../unsafe/composite.rkt")
(require "../unsafe/frame.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-geometry-group stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ Geo name base-op:expr sibs-op:expr
        (~optional (~seq #:outline outline) #:defaults ([outline #'#false]))
        layers0:expr argl:expr ...)
     (with-syntax ([geo-prefix (datum->syntax #'Geo (format "~a:" (syntax->datum #'Geo)))])
       (syntax/loc stx
         (let ([layers layers0])
           (Geo geo-convert geo-draw-group! (geo-group-extent layers)
                (or outline (geo-group-outline layers))
                (or name (gensym 'geo-prefix)) base-op sibs-op layers
                argl ...))))]
    [(_ Geo name base-op:expr sibs-op:expr
        (~alt (~optional (~seq #:outline alt-outline) #:defaults ([alt-outline #'#false]))
              (~optional (~seq #:margin margin) #:defaults ([margin #'#false]))
              (~optional (~seq #:padding inset) #:defaults ([inset #'#false]))
              (~optional (~seq #:border border) #:defaults ([border #'#false]))
              (~optional (~seq #:background bgsource) #:defaults ([bgsource #'#false])))
        ...
        layers0 argl ...)
     (with-syntax ([geo-prefix (datum->syntax #'Geo (format "~a:" (syntax->datum #'Geo)))])
       (syntax/loc stx
         (let* ([layers layers0]
                [outline (or alt-outline (geo-group-outline layers))]
                [id (or name (gensym 'geo-prefix))])
           (if (or margin inset border bgsource)
               (Geo geo-convert
                    (geo-draw-framed-group! border bgsource) (geo-group-frame-extent margin inset layers border) outline
                    id base-op sibs-op layers argl ...)
               (Geo geo-convert
                    geo-draw-group! (geo-group-extent layers) outline
                    id base-op sibs-op layers argl ...)))))]))

(define-syntax (create-geometry-table stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ Geo name base-op:expr sibs-op:expr
        (~optional (~seq #:outline outline) #:defaults ([outline #'#false]))
        table:expr ncols nrows col-anchors row-anchors col-gaps row-gaps argl:expr ...)
     (with-syntax ([geo-prefix (datum->syntax #'Geo (format "~a:" (syntax->datum #'Geo)))])
       (syntax/loc stx
         (let-values ([(layers size anchors gaps) (geo-table-metrics table ncols nrows col-anchors row-anchors col-gaps row-gaps)])
           (create-geometry-group Geo name base-op sibs-op
                                  #:outline outline
                                  layers size anchors gaps argl ...))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Frame-Blank-Datum (U Nonnegative-Real (Listof Nonnegative-Real)))
(define-type Geo-Frame-Border (Immutable-Vector Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum))

(struct geo:group geo
  ([base-operator : (Option Symbol)]
   [sibs-operator : (Option Symbol)]
   [selves : Geo-Layer-Group])
  #:type-name Geo:Group
  #:transparent)

(struct geo:table geo:group
  ([size : (Pairof Positive-Index Positive-Index)]
   [anchors : (Pairof (Vectorof Geo-Pin-Anchor) (Vectorof Geo-Pin-Anchor))]
   [gaps : (Pairof (Vectorof Flonum) (Vectorof Flonum))])
  #:type-name Geo:Table
  #:transparent)

(struct geo-frame-ink geo-ink
  ([body-origin : Float-Complex])
  #:type-name Geo-Frame-Ink
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-group-origin : (-> Geo:Group Float-Complex)
  (lambda [self]
    (define-values (W H ink) (geo-extent self))

    (if (geo-frame-ink? ink)
        (geo-frame-ink-body-origin ink)
        0.0+0.0i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-composite : (->* (Geo Real Real Geo)
                             (Real Real #:base-operator (Option Symbol) #:operator (Option Symbol) #:id (Option Symbol))
                             Geo:Group)
  (lambda [geo1 x1 y1 geo2 [x2 0.0] [y2 0.0] #:base-operator [base-op #false] #:operator [sibs-op #false] #:id [id #false]]
    (make-geo:group id base-op sibs-op
                    (geo-composite-layers geo1 geo2
                                          (- (real->double-flonum x1) (real->double-flonum x2))
                                          (- (real->double-flonum y1) (real->double-flonum y2))))))

(define geo-pin* : (-> Real Real Real Real Geo
                       [#:base-operator (Option Symbol)] [#:operator (Option Symbol)] [#:id (Option Symbol)]
                       Geo * Geo)
  (lambda [#:base-operator [base-op #false] #:operator [sibs-op #false] #:id [id #false] x1% y1% x2% y2% base . siblings]
    (cond [(null? siblings) base]
          [(null? (cdr siblings))
           (make-geo:group id base-op sibs-op
                           (geo-composite-layers base (car siblings)
                                                 (real->double-flonum x1%) (real->double-flonum y1%)
                                                 (real->double-flonum x2%) (real->double-flonum y2%)))]
          [else
           (make-geo:group id base-op sibs-op
                           (geo-pin-layers base siblings
                                           (real->double-flonum x1%) (real->double-flonum y1%)
                                           (real->double-flonum x2%) (real->double-flonum y2%)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-geo:group : (case-> [(Option Symbol) (Option Symbol) (Option Symbol) Geo-Layer-Group -> Geo:Group]
                                 [(Option Symbol) (Option Symbol) (Option Symbol) Geo-Layer-Group
                                                  (Option Geo-Frame-Blank-Datum) (Option Geo-Frame-Blank-Datum)
                                                  Maybe-Stroke-Paint Maybe-Fill-Paint
                                                  -> Geo:Group])
  (case-lambda
    [(id base-op sibs-op layers)
     (create-geometry-object geo:group
                             #:with [id geo-draw-group! (geo-group-extent layers) (geo-group-outline layers)]
                             base-op sibs-op layers)]
    [(id base-op sibs-op layers margin inset border background)
     (if (or margin inset border background)

         (let ([geo-frame-extent (geo-group-frame-extent margin inset layers border)])
           (create-geometry-object geo:group
                                   #:with [id (geo-draw-framed-group! border background) geo-frame-extent geo-zero-pads]
                                   base-op sibs-op layers))

         (make-geo:group id base-op sibs-op layers))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-siblings->table : (-> (Listof Geo) Positive-Integer (GLayerof Geo) (Vectorof (GLayerof Geo)))
  (lambda [siblings size cont]
    (list->n:vector* siblings size cont geo-own-layer)))

(define geo-siblings*->table : (-> (Listof (Listof (Option Geo))) Positive-Index (GLayerof Geo) (Vectorof (GLayerof Geo)))
  (lambda [siblings ncols cont]
    (define fill-row ((inst λoption Geo (GLayerof Geo)) geo-own-layer cont))

    (apply vector-append
           (for/list : (Listof (Vectorof (GLayerof Geo))) ([rows : (Listof (Option Geo)) (in-list siblings)])
             (list->n:vector* rows ncols cont fill-row)))))

(define geo-table-metrics : (-> (Vectorof (GLayerof Geo)) Positive-Index Positive-Index
                                (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Geo-Pin-Anchor)
                                (Geo-Config-Argof Real) (Geo-Config-Argof Real)
                                (Values Geo-Layer-Group
                                        (Pairof Positive-Index Positive-Index)
                                        (Pairof (Vectorof Geo-Pin-Anchor) (Vectorof Geo-Pin-Anchor))
                                        (Pairof (Vectorof Flonum) (Vectorof Flonum))))
  (lambda [table ncols nrows col-anchors row-anchors col-gaps row-gaps]
    (define pcols : (Vectorof Geo-Pin-Anchor) (geo-config-expand col-anchors ncols 'cc))
    (define prows : (Vectorof Geo-Pin-Anchor) (geo-config-expand row-anchors nrows 'cc))
    (define gcols : (Vectorof Flonum) (geo-config-expand col-gaps ncols 0.0 real->double-flonum))
    (define grows : (Vectorof Flonum) (geo-config-expand row-gaps nrows 0.0 real->double-flonum))
    
    (define size : Index (vector-length table))
    (define cwidths : (Vectorof Nonnegative-Flonum) (geo-table-column-widths table nrows ncols size))
    (define rheights : (Vectorof Nonnegative-Flonum) (geo-table-row-heights table nrows ncols size))
    (define layers : Geo-Layer-Group (geo-table-layers table ncols nrows pcols prows gcols grows cwidths rheights))

    (values layers (cons ncols nrows) (cons pcols prows) (cons gcols grows))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-group! : Geo-Surface-Draw!
  (lambda [self cr x0 y0 width height]
    (with-asserts ([self geo:group?])
      (geo_composite cr x0 y0 width height
                     (geo-operator->integer* (geo:group-base-operator self))
                     (geo-operator->integer* (geo:group-sibs-operator self))
                     (geo:group-selves self)))))

(define geo-draw-framed-group! : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-bdr alt-bg]
    (lambda [self cr x0 y0 width height]
      (with-asserts ([self geo:group?])
        (define-values (W H ink) (geo-extent self))

        (when (geo-frame-ink? ink)
          (let ([border (geo-ink-pos ink)]
                [origin (geo-frame-ink-body-origin ink)])
            (geo_framed_composite cr x0 y0 width height
                                  (geo-operator->integer* (geo:group-base-operator self))
                                  (geo-operator->integer* (geo:group-sibs-operator self))
                                  (geo:group-selves self) (real-part origin) (imag-part origin)
                                  (real-part border) (imag-part border) (geo-ink-width ink) (geo-ink-height ink)
                                  (geo-select-border-paint alt-bdr) (geo-select-background-source alt-bg))))))))

(define geo-group-extent : (-> Geo-Layer-Group Geo-Calculate-Extent)
  (lambda [layers]
    (define w (glayer-group-width layers))
    (define h (glayer-group-height layers))
    
    (λ [self]
      (values w h #false))))

(define geo-group-frame-extent : (-> (Option Geo-Frame-Blank-Datum) (Option Geo-Frame-Blank-Datum) Geo-Layer-Group Maybe-Stroke-Paint Geo-Calculate-Extent)
  (lambda [margin inset layers maybe-border]
    (define-values (mtop mright mbottom mleft)
      (cond [(list? margin) (list->4:values (map real->double-flonum margin) 0.0)]
            [(real? margin) (let ([fl (real->double-flonum margin)]) (values fl fl fl fl))]
            [else (values 0.0 0.0 0.0 0.0)]))
    
    (define-values (ptop pright pbottom pleft)
      (cond [(list? inset) (list->4:values (map real->double-flonum inset) 0.0)]
            [(real? inset) (let ([fl (real->double-flonum inset)]) (values fl fl fl fl))]
            [else (values 0.0 0.0 0.0 0.0)]))

    (define frame-outline : Geo-Calculate-Outline (geo-group-outline layers))
    (define-values (gwidth gheight) (values (glayer-group-width layers) (glayer-group-height layers)))

    (λ [[self : Geo<%>]]
      (define frame-pads : Geo-Pad (frame-outline self (current-stroke-source) (current-border-source)))
      (define-values (dx dy body-width body-height) (geo-pad-expand frame-pads gwidth gheight))
      
      (define-values (W H bx by border-width border-height ox oy)
        (dc_frame_size body-width body-height
                       mtop mright mbottom mleft ptop pright pbottom pleft
                       (geo-select-border-paint maybe-border)))
      
      (values W H
              (geo-frame-ink (make-rectangular bx by)
                             border-width border-height
                             (make-rectangular (+ ox dx) (+ oy dy)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-group-outline : (-> Geo-Layer-Group Geo-Calculate-Outline)
  (lambda [layers]
    (define W (glayer-group-width layers))
    (define H (glayer-group-height layers))
    
    (λ [[master : Geo<%>] [stroke : Option-Stroke-Paint] [border : Option-Stroke-Paint]] : Geo-Pad
      (let check-boundary ([lx : Flonum 0.0]
                           [ty : Flonum 0.0]
                           [rx : Nonnegative-Flonum W]
                           [by : Nonnegative-Flonum H]
                           [siblings : (Listof (GLayerof Geo)) (glayer-group-layers layers)])
        (cond [(pair? siblings)
               (let*-values ([(self rest) (values (car siblings) (cdr siblings))]
                             [(x y w h) (values (glayer-x self) (glayer-y self) (glayer-width self) (glayer-height self))]
                             [(outline) (geo-outline* (glayer-master self) stroke border)]
                             [(l t) (values (geo-pad-left outline) (geo-pad-top outline))]
                             [(r b) (values (geo-pad-right outline) (geo-pad-bottom outline))])
                 (check-boundary (min lx (- x l)) (min ty (- y t)) (max rx (+ x w r)) (max by (+ y h b)) rest))]
              [(or (< lx 0.0) (< ty 0.0) (> rx W) (> by H))
               (geo-pad (abs ty) (abs (- rx W)) (abs (- by H)) (abs lx))]
              [else geo-zero-pads])))))

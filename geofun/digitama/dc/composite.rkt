#lang typed/racket/base

(provide (all-defined-out))

(require racket/vector)
(require digimon/sequence)
(require digimon/function)
(require digimon/measure)

(require "../self.rkt")
(require "../convert.rkt")

(require "../paint.rkt")
(require "../composite.rkt")

(require "../geometry/ink.rkt")
(require "../geometry/insets.rkt")
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
        (~alt (~optional (~seq #:outline outline) #:defaults ([outline #'#false]))
              (~optional (~seq #:desc desc) #:defaults ([desc #'#false]))) ...
        layers0:expr argl:expr ...)
     (with-syntax ([geo-prefix (datum->syntax #'Geo (format "~a:" (syntax->datum #'Geo)))])
       (syntax/loc stx
         (let ([layers layers0])
           (Geo geo-convert geo-draw-group! (geo-group-extent layers)
                (or outline (geo-group-outline layers))
                (or name (gensym 'geo-prefix)) base-op sibs-op desc layers
                argl ...))))]
    [(_ Geo name base-op:expr sibs-op:expr
        (~alt (~optional (~seq #:outline alt-outline) #:defaults ([alt-outline #'#false]))
              (~optional (~seq #:desc desc) #:defaults ([desc #'#false]))
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
                    id base-op sibs-op desc layers argl ...)
               (Geo geo-convert
                    geo-draw-group! (geo-group-extent layers) outline
                    id base-op sibs-op desc layers argl ...)))))]))

(define-syntax (create-geometry-table stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ Geo name base-op:expr sibs-op:expr
        (~alt (~optional (~seq #:outline outline) #:defaults ([outline #'#false]))
              (~optional (~seq #:desc desc) #:defaults ([desc #'#false]))
              (~optional (~seq #:margin margin) #:defaults ([margin #'#false]))
              (~optional (~seq #:padding inset) #:defaults ([inset #'#false]))
              (~optional (~seq #:border border) #:defaults ([border #'#false]))
              (~optional (~seq #:background bgsource) #:defaults ([bgsource #'#false])))
        ...
        table:expr ncols nrows col-anchors row-anchors col-gaps row-gaps argl:expr ...)
     (with-syntax ([geo-prefix (datum->syntax #'Geo (format "~a:" (syntax->datum #'Geo)))])
       (syntax/loc stx
         (let-values ([(layers size anchors gaps) (geo-table-metrics table ncols nrows col-anchors row-anchors col-gaps row-gaps)])
           (create-geometry-group Geo name base-op sibs-op
                                  #:outline outline #:desc desc
                                  #:margin margin #:padding inset #:border border #:background bgsource
                                  layers size anchors gaps argl ...))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:group geo
  ([base-operator : (Option Symbol)]
   [sibs-operator : (Option Symbol)]
   [desc : (Option String)]
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
                             (#:base-operator (Option Symbol) #:operator (Option Symbol)
                              #:id (Option Symbol) #:desc (Option String)
                              Real Real)
                             Geo:Group)
  (lambda [#:base-operator [base-op #false] #:operator [sibs-op #false]
           #:id [id #false] #:desc [desc #false]
           geo1 x1 y1 geo2 [x2 0.0] [y2 0.0]]
    (make-geo:group id base-op sibs-op desc
                    (geo-composite-layers geo1 geo2
                                          (- (real->double-flonum x1) (real->double-flonum x2))
                                          (- (real->double-flonum y1) (real->double-flonum y2))))))

(define geo-pin* : (-> Real Real Real Real Geo
                       [#:id (Option Symbol)] [#:desc (Option String)]
                       [#:base-operator (Option Symbol)] [#:operator (Option Symbol)]
                       Geo * Geo)
  (lambda [#:base-operator [base-op #false] #:operator [sibs-op #false] #:id [id #false] #:desc [desc #false]
           x1% y1% x2% y2% base . siblings]
    (cond [(null? siblings) base]
          [(null? (cdr siblings))
           (make-geo:group id base-op sibs-op desc
                           (geo-composite-layers base (car siblings)
                                                 (real->double-flonum x1%) (real->double-flonum y1%)
                                                 (real->double-flonum x2%) (real->double-flonum y2%)))]
          [else
           (make-geo:group id base-op sibs-op desc
                           (geo-pin-layers base siblings
                                           (real->double-flonum x1%) (real->double-flonum y1%)
                                           (real->double-flonum x2%) (real->double-flonum y2%)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-geo:group : (case-> [(Option Symbol) (Option Symbol) (Option Symbol) (Option String) Geo-Layer-Group -> Geo:Group]
                                 [(Option Symbol) (Option Symbol) (Option Symbol) (Option String) Geo-Layer-Group
                                                  (Option Geo-Insets-Datum) (Option Geo-Insets-Datum)
                                                  Maybe-Stroke-Paint Maybe-Fill-Paint
                                                  -> Geo:Group])
  (case-lambda
    [(id base-op sibs-op desc layers)
     (create-geometry-object geo:group
                             #:with [id geo-draw-group! (geo-group-extent layers) (geo-group-outline layers)]
                             base-op sibs-op desc layers)]
    [(id base-op sibs-op desc layers margin inset border background)
     (if (or margin inset border background)

         (let ([geo-frame-extent (geo-group-frame-extent margin inset layers border)])
           (create-geometry-object geo:group
                                   #:with [id (geo-draw-framed-group! border background) geo-frame-extent geo-zero-pads]
                                   base-op sibs-op desc layers))

         (make-geo:group id base-op sibs-op desc layers))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-siblings->table : (-> (Listof Geo) Positive-Integer (GLayerof Geo) (Vectorof (GLayerof Geo)))
  (lambda [siblings size placeholder]
    (list->n:vector* siblings size placeholder geo-own-layer)))

(define geo-siblings*->table : (-> (Listof (Listof (Option Geo))) Positive-Index (GLayerof Geo) (Vectorof (GLayerof Geo)))
  (lambda [siblings ncols placeholder]
    (define fill-row ((inst λoption Geo (GLayerof Geo)) geo-own-layer placeholder))

    (apply vector-append
           (for/list : (Listof (Vectorof (GLayerof Geo))) ([row (in-list siblings)])
             (list->n:vector* row ncols placeholder fill-row)))))

(define geo-table-metrics : (-> (Vectorof (GLayerof Geo)) Positive-Index Positive-Index
                                (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Geo-Pin-Anchor)
                                (Geo-Config-Argof Real-Length) (Geo-Config-Argof Real-Length)
                                (Values Geo-Layer-Group
                                        (Pairof Positive-Index Positive-Index)
                                        (Pairof (Vectorof Geo-Pin-Anchor) (Vectorof Geo-Pin-Anchor))
                                        (Pairof (Vectorof Flonum) (Vectorof Flonum))))
  (lambda [table ncols nrows col-anchors row-anchors col-gaps row-gaps]
    (define pcols : (Vectorof Geo-Pin-Anchor) (geo-config-expand col-anchors ncols 'cc))
    (define prows : (Vectorof Geo-Pin-Anchor) (geo-config-expand row-anchors nrows 'cc))
    (define gcols : (Vectorof Flonum) (geo-config-expand col-gaps ncols 0.0 ~distance))
    (define grows : (Vectorof Flonum) (geo-config-expand row-gaps nrows 0.0 ~distance))
    
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

(define geo-group-frame-extent : (-> (Option Geo-Insets-Datum) (Option Geo-Insets-Datum) Geo-Layer-Group Maybe-Stroke-Paint Geo-Calculate-Extent)
  (lambda [margin inset layers maybe-border]
    (define-values (mtop mright mbottom mleft) (geo-inset-values margin))
    (define-values (ptop pright pbottom pleft) (geo-inset-values inset))

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

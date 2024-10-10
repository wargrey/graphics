#lang typed/racket/base

(provide (all-defined-out))

(require digimon/sequence)

(require "ink.rkt")
(require "paint.rkt")
(require "../../paint.rkt")

(require "../convert.rkt")
(require "../composite.rkt")

(require "../layer/type.rkt")
(require "../layer/combine.rkt")
(require "../layer/table.rkt")

(require "../unsafe/composite.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-geometry-group stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ Geo (~optional (~seq #:id name) #:defaults ([name #'#false])) ...
        op:expr layers:expr argl:expr ...)
     (with-syntax ([geo-prefix (datum->syntax #'Geo (format "~a:" (syntax->datum #'Geo)))])
       (syntax/loc stx
         (Geo geo-convert geo-group-surface (geo-group-extent layers)
              (or name (gensym 'geo-prefix)) op layers
              geo-frame-empty geo-frame-empty
              argl ...)))]
    [(_ Geo
        (~alt (~optional (~seq #:id name) #:defaults ([name #'#false]))
              (~optional (~seq #:margin margin) #:defaults ([margin #'#false]))
              (~optional (~seq #:padding inset) #:defaults ([inset #'#false]))
              (~optional (~seq #:border bdr) #:defaults ([bdr #'#false]))
              (~optional (~seq #:background bgsource) #:defaults ([bgsource #'#false])))
        ...
        op layers argl ...)
     (with-syntax ([geo-prefix (datum->syntax #'Geo (format "~a:" (syntax->datum #'Geo)))])
       (syntax/loc stx
         (let-values ([(geo-frame-extent margins insets) (geo-group-frame-extent margin inset layers bdr)])
           (Geo geo-convert (geo-framed-group-surface bdr bgsource) geo-frame-extent
                (or name (gensym 'geo-prefix)) op layers margins insets
                argl ...))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Frame-Blank-Datum (U Nonnegative-Real (Listof Nonnegative-Real)))
(define-type Geo-Frame-Blank (Immutable-Vector Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum))

(define geo-frame-empty : Geo-Frame-Blank #(0.0 0.0 0.0 0.0))

(struct geo:group geo
  ([operator : (Option Symbol)]
   [selves : Geo-Layer-Group]
   [margins : Geo-Frame-Blank]
   [pads :  Geo-Frame-Blank])
  #:type-name Geo:Group
  #:transparent)

(struct geo:table geo:group
  ([size : (Pairof Positive-Index Positive-Index)]
   [anchors : (Pairof (Vectorof Geo-Pin-Anchor) (Vectorof Geo-Pin-Anchor))]
   [gaps : (Pairof (Vectorof Flonum) (Vectorof Flonum))])
  #:type-name Geo:Table
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-composite : (->* (Geo Real Real Geo) (Real Real #:operator (Option Symbol) #:id (Option Symbol)) Geo:Group)
  (lambda [geo1 x1 y1 geo2 [x2 0.0] [y2 0.0] #:operator [op #false] #:id [id #false]]
    (make-geo:group id op
                    (geo-composite-layers geo1 geo2
                                          (- (real->double-flonum x1) (real->double-flonum x2))
                                          (- (real->double-flonum y1) (real->double-flonum y2))))))

(define geo-pin* : (-> Real Real Real Real Geo [#:operator (Option Symbol)] [#:id (Option Symbol)] Geo * Geo)
  (lambda [#:operator [op #false] #:id [id #false] x1% y1% x2% y2% base . siblings]
    (cond [(null? siblings) base]
          [(null? (cdr siblings))
           (make-geo:group id op
                           (geo-composite-layers base (car siblings)
                                                 (real->double-flonum x1%) (real->double-flonum y1%)
                                                 (real->double-flonum x2%) (real->double-flonum y2%)))]
          [else
           (make-geo:group id op
                           (geo-pin-layers base siblings
                                           (real->double-flonum x1%) (real->double-flonum y1%)
                                           (real->double-flonum x2%) (real->double-flonum y2%)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-geo:group : (case-> [(Option Symbol) (Option Symbol) Geo-Layer-Group -> Geo:Group]
                                 [(Option Symbol) (Option Symbol) Geo-Layer-Group
                                                  (Option Geo-Frame-Blank-Datum) (Option Geo-Frame-Blank-Datum)
                                                  Maybe-Stroke-Paint Maybe-Fill-Paint
                                                  -> Geo:Group])
  (case-lambda
    [(id op layers)
     (create-geometry-object geo:group
                             #:surface geo-group-surface
                             #:extent (geo-group-extent layers)
                             #:id id
                             op layers geo-frame-empty geo-frame-empty)]
    [(id op layers margin inset border background)
     (if (or margin inset border background)

         (let-values ([(geo-frame-extent margins insets) (geo-group-frame-extent margin inset layers border)])   
           (create-geometry-object geo:group
                                   #:surface (geo-framed-group-surface border background)
                                   #:extent geo-frame-extent
                                   #:id id
                                   op layers margins insets))

         (make-geo:group id op layers))]))

(define make-geo:table : (-> (Option Symbol) (Option Symbol)
                             (Listof Geo) Positive-Index Positive-Index
                             (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Real) (Geo-Config-Argof Real) Geo
                             Geo:Table)
  (lambda [id op siblings ncols nrows col-anchors row-anchors col-gaps row-gaps cont]
    (define pcols : (Vectorof Geo-Pin-Anchor) (geo-config-expand col-anchors ncols 'cc))
    (define prows : (Vectorof Geo-Pin-Anchor) (geo-config-expand row-anchors nrows 'cc))
    (define gcols : (Vectorof Flonum) (geo-config-expand col-gaps ncols 0.0 real->double-flonum))
    (define grows : (Vectorof Flonum) (geo-config-expand row-gaps nrows 0.0 real->double-flonum))
    (define table : (Vectorof (GLayerof Geo)) (list->n:vector* siblings (* nrows ncols) (geo-own-layer cont) geo-own-layer))

    (define size : Index (vector-length table))
    (define cwidths : (Vectorof Nonnegative-Flonum) (geo-table-column-widths table nrows ncols size))
    (define rheights : (Vectorof Nonnegative-Flonum) (geo-table-row-heights table nrows ncols size))
    (define layers : Geo-Layer-Group (geo-table-layers table ncols nrows pcols prows gcols grows cwidths rheights))

    (create-geometry-group geo:table #:id id
                           op layers (cons ncols nrows) (cons pcols prows) (cons gcols grows))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-group-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:group?])
      (geo_composite (geo-select-operator (geo:group-operator self) default-pin-operator)
                     (geo:group-selves self) (default-geometry-density)))))

(define geo-framed-group-surface : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Create)
  (lambda [alt-bdr alt-bg]
    (lambda [self]
      (with-asserts ([self geo:group?])
        (define margins (geo:group-margins self))
        (define pads (geo:group-pads self))
        (define-values (mtop mright mbottom mleft ptop pright pbottom pleft)
          (values (vector-ref margins 0) (vector-ref margins 1) (vector-ref margins 2) (vector-ref margins 3)
                  (vector-ref pads 0)    (vector-ref pads 1)    (vector-ref pads 2)    (vector-ref pads 3)))
    
        (geo_framed_composite (geo-select-operator (geo:group-operator self) default-pin-operator) (geo:group-selves self)
                              mtop mright mbottom mleft ptop pright pbottom pleft
                              (geo-select-border-paint alt-bdr) (geo-select-background-source alt-bg)
                              (default-geometry-density))))))

(define geo-group-extent : (-> Geo-Layer-Group Geo-Calculate-Extent)
  (lambda [layers]
    (define w (glayer-group-width layers))
    (define h (glayer-group-height layers))
    
    (λ [self]
      (values w h #false))))

(define geo-group-frame-extent : (-> (Option Geo-Frame-Blank-Datum) (Option Geo-Frame-Blank-Datum) Geo-Layer-Group Maybe-Stroke-Paint
                                     (Values Geo-Calculate-Extent Geo-Frame-Blank Geo-Frame-Blank))
  (lambda [margin inset layers border]
    (define-values (mtop mright mbottom mleft)
      (cond [(list? margin) (list->4:values (map real->double-flonum margin) 0.0)]
            [(real? margin) (let ([fl (real->double-flonum margin)]) (values fl fl fl fl))]
            [else (values 0.0 0.0 0.0 0.0)]))
    
    (define-values (ptop pright pbottom pleft)
      (cond [(list? inset) (list->4:values (map real->double-flonum inset) 0.0)]
            [(real? inset) (let ([fl (real->double-flonum inset)]) (values fl fl fl fl))]
            [else (values 0.0 0.0 0.0 0.0)]))
    
    (define-values (flwidth flheight) (values (glayer-group-width layers) (glayer-group-height layers)))
    
    (define geo-frame-extent : Geo-Calculate-Extent
      (lambda [self]
        (define-values (W H lx ty w h)
          (dc_frame_size flwidth flheight
                         mtop mright mbottom mleft ptop pright pbottom pleft
                         (geo-select-border-paint border)))
        (values W H (make-geo-ink lx ty w h))))
    
    (values geo-frame-extent
            (vector-immutable mtop mright mbottom mleft)
            (vector-immutable ptop pright pbottom pleft))))

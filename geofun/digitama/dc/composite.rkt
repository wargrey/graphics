#lang typed/racket/base

(provide (all-defined-out))

(require digimon/sequence)

(require "../../paint.rkt")
(require "../convert.rkt")
(require "../composite.rkt")
(require "../unsafe/composite.rkt")
(require "../layer/type.rkt")
(require "../layer/combine.rkt")
(require "../layer/table.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (create-geometry-group stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ Geo (~optional (~seq #:id name) #:defaults ([name #'#false])) op layers argl ...)
     (with-syntax ([geo-prefix (datum->syntax #'Geo (format "~a:" (syntax->datum #'Geo)))])
       (syntax/loc stx
         (Geo geo-convert geo-group-surface (geo-group-extent layers)
              (or name (gensym 'geo-prefix))
              op layers argl ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:group geo
  ([operator : (Option Symbol)]
   [selves : Geo-Layer-Group])
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
(define make-geo:group : (-> (Option Symbol) (Option Symbol) Geo-Layer-Group Geo:Group)
  (lambda [id op layers]
    (create-geometry-object geo:group
                            #:surface geo-group-surface
                            #:extent (geo-group-extent layers)
                            #:id id
                            op layers)))

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

(define geo-group-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:group?])
      (geo_composite (geo-select-operator (geo:group-operator self) default-pin-operator)
                     (geo:group-selves self)
                     (default-geometry-density)))))

(define geo-group-extent : (-> Geo-Layer-Group Geo-Calculate-Extent)
  (lambda [layers]
    (define w (glayer-group-width layers))
    (define h (glayer-group-height layers))
    
    (λ [self]
      (values w h #false))))

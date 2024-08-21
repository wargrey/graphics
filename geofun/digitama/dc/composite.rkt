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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:group geo
  ([operator : (Option Symbol)]
   [layers : Geo-Layer-Group])
  #:type-name Geo:Group
  #:transparent)

(struct geo:table geo:group
  ([size : (Pairof Positive-Index Positive-Index)]
   [ports : (Pairof (Vectorof Geo-Pin-Port) (Vectorof Geo-Pin-Port))]
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
                            #:with [geo-group-surface (geo-group-bbox layers)] #:id id
                            op layers)))

(define make-geo:table : (-> (Option Symbol) (Option Symbol)
                             (Listof Geo) Positive-Index Positive-Index
                             (Geo-Config-Argof Geo-Pin-Port) (Geo-Config-Argof Geo-Pin-Port) (Geo-Config-Argof Real) (Geo-Config-Argof Real) Geo
                             Geo:Table)
  (lambda [id op siblings ncols nrows col-ports row-ports col-gaps row-gaps cont]
    (define pcols : (Vectorof Geo-Pin-Port) (geo-config-expand col-ports ncols 'cc))
    (define prows : (Vectorof Geo-Pin-Port) (geo-config-expand row-ports nrows 'cc))
    (define gcols : (Vectorof Flonum) (geo-config-expand col-gaps ncols 0.0 real->double-flonum))
    (define grows : (Vectorof Flonum) (geo-config-expand row-gaps nrows 0.0 real->double-flonum))
    (define table : (Vectorof (GLayerof Geo)) (list->n:vector* siblings (* nrows ncols) (geo->layer cont) geo->layer))

    (define size : Index (vector-length table))
    (define cwidths : (Vectorof Nonnegative-Flonum) (geo-table-column-widths table nrows ncols size))
    (define rheights : (Vectorof Nonnegative-Flonum) (geo-table-row-heights table nrows ncols size))
    (define layers : Geo-Layer-Group (geo-table-layers table ncols nrows pcols prows gcols grows cwidths rheights))

    (create-geometry-object geo:table
                            #:with [geo-group-surface (geo-group-bbox layers)] #:id id
                            op layers
                            (cons ncols nrows) (cons pcols prows) (cons gcols grows))))

(define geo-group-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:group?])
      (geo_composite (geo-select-operator (geo:group-operator self) default-pin-operator)
                     (geo:group-layers self) 
                     (default-geometry-density)))))

(define geo-group-bbox : (-> Geo-Layer-Group Geo-Calculate-BBox)
  (lambda [layers]
    (define w (vector-ref layers 0))
    (define h (vector-ref layers 1))
    
    (λ [self]
      (values 0.0 0.0 w h))))
